{- |
Module: Data.ORef.Internal

Internal types and functions
-}

module Data.ORef.Internal
  ( ORef(ORef)
  , Own
  , Entry(..)
  , incBC
  , decBC
  , hasBorrowers
  , hasOtherBorrowers
  -- , readFlag
  -- , writeFlag
  -- , setEntryReadFlag
  , setEntryWriteFlag
  , checkEntryReadFlag
  , checkEntryWriteFlag
  -- , checkEntry
  , checkORef
  -- , checkThreadId
  , value
  , getEntry
  -- , deleteEntry
  -- , getReadFlag
  -- , getWriteFlag
  , setReadFlag
  , setWriteFlag
  , getValue
  , setValue
  -- running the monad
  , forkOwn
  , evalOwn
  , startOwn
  , execOwn
  ) where

import Prelude hiding (lookup)

import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Concurrent

import Data.Typeable (Typeable,cast)
import Data.IntMap (IntMap, empty, lookup, insert, adjust)

-- | A typed reference to an owned value.
data ORef a = ORef ID

-- | Ownership Monad with IO in the transformers stack
type Own a = StateT (ID,Store) (EitherT String IO) a

-- | Each ORef has a unique ID.
type ID = Int

-- | Store that maps IDs to entries.
type Store = IntMap Entry

type Readable = Bool
type Writeable = Bool
type BorrowerCount = Int

-- | An entry in the store is
-- a boolean flag indicating whether this ORef can be read,
-- a boolean flag indicating whether this ORef can be written to,
-- the number of borrowers
-- the threadID of the owner,
-- and a value of arbitrary type.
data Entry =
  forall v. Typeable v => Entry Readable Writeable BorrowerCount ThreadId v


-- | Get the borrower count for an Entry
borrowerCount :: Entry -> Int
borrowerCount (Entry _r _w bc _thID _v) = bc

-- | Set the borrrower count for an Entry
setEntryBorrowerCount :: Int -> Entry -> Entry
setEntryBorrowerCount bc (Entry r w _ thID v) = (Entry r w bc thID v)

-- | Increase the borrower count by a certain number
addToBorrowerCount :: Int -> Entry -> Entry
addToBorrowerCount newB (Entry r w bc thID v) = (Entry r w (bc + newB) thID v)

-- | Decrease the borrower count by a certain number
reduceBorrowerCount :: Int -> Entry -> Entry
reduceBorrowerCount lessB (Entry r w bc thID v) = (Entry r w (bc - lessB) thID v)

incBC :: ORef a -> Own ()
incBC oref = adjustEntry oref (addToBorrowerCount 1)

decBC :: ORef a -> Own ()
decBC oref = adjustEntry oref (reduceBorrowerCount 1)


hasBorrowers :: ORef a -> Own Bool
hasBorrowers oref = do
  bc <- getBorrowerCount oref
  return (bc > 0)

-- | hasOtherBorrowers will return if there are other borrowers
-- other than the ORef calling the function - this assumes
-- that hasOtherBorrowers will be used by a borrower
hasOtherBorrowers :: ORef a -> Own Bool
hasOtherBorrowers oref = do
  bc <- getBorrowerCount oref
  return (bc - 1 > 0)

-- | The flag of an entry.
readFlag :: Entry -> Bool
readFlag (Entry r _w _ _ _) = r

writeFlag :: Entry -> Bool
writeFlag (Entry _r w _ _ _) = w

-- | Adjust the flag of an Entry to the given flag
setEntryReadFlag :: Bool -> Entry -> Entry
setEntryReadFlag b (Entry _ w bc t a) = (Entry b w bc t a)

-- | Adjust the flag of an Entry to the given flag
setEntryWriteFlag :: Bool -> Entry -> Entry
setEntryWriteFlag b (Entry r _ bc t a) = (Entry r b bc t a)

-- | Check if an Entry can be read and if it is in the same thread
checkEntryReadFlag :: Entry -> IO Bool
checkEntryReadFlag (Entry r _w _bc thrId _) = do
  threadId <- myThreadId
  return $ (threadId == thrId) && r

-- | Check if an Entry can be written to and if it is in the same thread
checkEntryWriteFlag :: Entry -> IO Bool
checkEntryWriteFlag (Entry _r w _bc thrId _) = do
  threadId <- myThreadId
  return $ (threadId == thrId) && w

-- | Check if the entry is able to be read and written to.
--
-- This will also check if the thread ID of the current thread matches the
-- thread specified in the Entry.
-- and if the Entry has more than 0 borrowers.
--
-- Checking the thread is done to prevent a child from using ORef's that it
-- inherited from its parent but was not explicitely given.
checkEntry :: Entry -> IO Bool
checkEntry (Entry r w bc thrId _) = do
  threadId <- myThreadId
  return $ (threadId == thrId) && r && w && (bc <= 0)

-- | Check if the ORef is able to be read and written to.
--
-- This will also check if the thread ID of the current thread matches the
-- thread specified in the ORef.
-- and if the ORef has more than 0 borrowers.
--
-- Checking the thread is done to prevent a child from using ORef's that it
-- inherited from its parent but was not explicitely given.
checkORef :: ORef a -> Own Bool
checkORef oref  = do
  entry <- getEntry oref
  ok <- liftIO $ checkEntry entry
  return ok

-- | The value of an entry casted to the expected type.
value :: Typeable a => Entry -> a
value (Entry _ _ _ _ v) = case cast v of
    Just a  -> a
    Nothing -> error "internal cast error"

-- | Modify the current store.
modifyStore :: (Store -> Store) -> Own ()
modifyStore f = modify (\(n,s) -> (n, f s))

-- | Get an entry from the store or fail if no such entry exists.
getEntry :: ORef a -> Own Entry
getEntry (ORef i) = get >>= maybe err return . lookup i . snd
  where err = error ("entry not found: " ++ show i)

-- | Set an entry in the store.
setEntry :: Typeable a => ORef a -> Bool -> Bool -> Int -> a -> Own ()
setEntry (ORef i) r w bc a = do
  thrId <- liftIO $ myThreadId
  modifyStore (insert i (Entry r w bc thrId a))

-- | Adjust an entry in current store
--
-- This is similiar to modifyStore
adjustEntry :: ORef a -> (Entry -> Entry) -> Own ()
adjustEntry (ORef i) k = modifyStore (adjust k i)

-- | Delete an entry from the store.
-- deleteEntry :: ORef a -> Own ()
-- deleteEntry (ORef i) = modifyStore (delete i)

-- | Get the current flag for reading an ORef.
getReadFlag :: ORef a -> Own Bool
getReadFlag oref = fmap readFlag (getEntry oref)

-- | Get the current flag for writing to an ORef.
getWriteFlag :: ORef a -> Own Bool
getWriteFlag oref = fmap writeFlag (getEntry oref)

-- | Get the current flag for writing to an ORef.
getBorrowerCount :: ORef a -> Own Int
getBorrowerCount oref = fmap borrowerCount (getEntry oref)

-- | Set the read flag for an ORef.
setReadFlag :: ORef a -> Bool -> Own ()
setReadFlag oref b = adjustEntry oref (setEntryReadFlag b)

-- | Set the write flag for an ORef.
setWriteFlag :: ORef a -> Bool -> Own ()
setWriteFlag oref b = adjustEntry oref (setEntryWriteFlag b)

-- | Get the current value of an ORef.
getValue :: Typeable a => ORef a -> Own a
getValue oref = fmap value (getEntry oref)

-- | Set the current value of an ORef.
--
-- This will set the value - it will __NOT__ check ownership or thread
setValue :: Typeable a => ORef a -> a -> Own ()
setValue oref a = do
    r <- getReadFlag oref
    w <- getWriteFlag oref
    bc <- getBorrowerCount oref
    setEntry oref r w bc a


-- TODO get rid of continueOwn?
-- | Evaluate the Ownership monad operations within the context of
-- an existing Ownership monad.
--
-- This will use the state from the previous context.
continueOwn :: Own a -> Own (Either String a)
continueOwn x = do
  s <- get
  liftIO $ runEitherT (evalStateT x s)

-- | Create a child thread that uses the state of the parent thread's ownership
-- context when evaluating it's ownership operations in the child thread
forkOwn :: Show a => Own a -> Own ()
forkOwn innerOps = do
  parentState <- get -- get the parent state before forking
  _ <- liftIO $ forkIO $ do
    -- within IO
    childResult <- evalOwn innerOps parentState
    putStrLn $ "The child result was " ++ (show childResult)
    return ()
  return ()

-- | Run an action in the ownership monad and return its result.
--
-- Evaluate an ownership computation with the initial context passed as an
-- argument.
evalOwn :: Monad m =>
  StateT s (EitherT String m) a1 ->
  s ->
  m (Either String a1)
evalOwn actions startState =
  runEitherT (evalStateT actions startState)

-- | Run an action in the ownership monad and return its result.
--
-- This will run the action in an initially empty context
startOwn :: (Num t, Monad m) =>
  StateT (t, IntMap a) (EitherT String m) a1 ->
  m (Either String a1)
startOwn x = runEitherT (evalStateT x (0, empty))

-- TODO get rid of? Not clear this is useful
--
-- | Evaluate a state computation with a given state and return the final state,
-- discarding the final value
--
-- execOwn :: Monad m => Own a -> s -> m s
execOwn :: Monad m => StateT a1 (EitherT e m) a -> a1 -> m (Either e a1)
execOwn x s = runEitherT (execStateT x s)
