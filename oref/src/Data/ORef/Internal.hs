{- |
Module: Data.ORef.Internal

Internal types and functions
-}
module Data.ORef.Internal
  ( ORef(ORef)
  , Own
  , Entry(..)
  , setEntryWriteFlag
  , checkEntryReadFlag
  , checkEntryWriteFlag
  , checkORef
  , getEntry
  -- , setEntry
  , setReadFlag
  , setWriteFlag
  , getValue
  , setValue
  , setValueEmpty
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

import Data.Typeable (Typeable, cast)
import Data.IORef (IORef, newIORef, readIORef)
import Data.IntMap (IntMap, empty, lookup, insert, adjust)

-- | A typed reference to an owned value.
newtype ORef a = ORef ID

-- | Ownership Monad with IO in the transformers stack
type Own a = StateT (ID,Store) (EitherT String IO) a

-- | Each ORef has a unique ID.
type ID = Int

-- | Store that maps IDs to entries.
type Store = IntMap Entry

type Readable = Bool
type Writeable = Bool

-- | An Entry in the Ownership Monad
--
-- An entry in the store is
-- a boolean flag indicating whether this ORef can be read,
-- a boolean flag indicating whether this ORef can be written to,
-- the threadID of the owner,
-- and a value of arbitrary type.
--
-- Values in the Entry datatype are wrapped in the Maybe datatype to represent
-- empty entries.
--
data Entry =
  forall v. Typeable v => Entry Readable Writeable ThreadId (Maybe (IORef v))

-- | The read flag of an entry.
readFlag :: Entry -> Bool
readFlag (Entry r _w _ _ ) = r

-- | The write flag of an entry
writeFlag :: Entry -> Bool
writeFlag (Entry _r w _ _) = w

-- | Adjust the flag of an Entry to the given flag
setEntryReadFlag :: Bool -> Entry -> Entry
setEntryReadFlag b (Entry _ w t v) = (Entry b w t v)

-- | Adjust the flag of an Entry to the given flag
setEntryWriteFlag :: Bool -> Entry -> Entry
setEntryWriteFlag b (Entry r _ t v) = (Entry r b t v)

-- | Check if the entry is empty
--
-- This will return True if the entry is empty.
entryIsEmpty :: Entry -> Bool
entryIsEmpty (Entry _r _w _thrId Nothing)  = True
entryIsEmpty (Entry _r _w _thrId (Just _)) = False

-- | Check if an Entry can be read and if it is in the same thread
checkEntryReadFlag :: Entry -> IO Bool
checkEntryReadFlag (Entry r _w thrId _) = do
  threadId <- myThreadId
  return $ (threadId == thrId) && r

-- | Check if an Entry can be written to and if it is in the same thread
checkEntryWriteFlag :: Entry -> IO Bool
checkEntryWriteFlag (Entry _r w thrId _) = do
  threadId <- myThreadId
  return $ (threadId == thrId) && w

-- | Check if the entry is able to be read and written to.
--
-- This will also check if the thread ID of the current thread matches the
-- thread specified in the Entry.
--
-- Checking the thread is done to prevent a child from using ORef's that it
-- inherited from its parent but was not explicitely given.
checkEntry :: Entry -> IO Bool
checkEntry (Entry r w thrId _) = do
  threadId <- myThreadId
  return $ (threadId == thrId) && r && w

-- | Check if the ORef is able to be read and written to.
--
-- This will also check if the thread ID of the current thread matches the
-- thread specified in the ORef.
--
-- Checking the thread is done to prevent a child from using ORef's that it
-- inherited from its parent but was not explicitely given.
checkORef :: ORef a -> Own Bool
checkORef oref  = do
  entry <- getEntry oref
  ok <- liftIO $ checkEntry entry
  return ok
-- TODO should this also include if the ORef Entry is an empty Nothing value?

-- | The value inside the IORef of an entry casted to the expected type.
--
-- If the value of an entry is Nothing then Nothing will be returned.
--
-- This function will fail if there is a cast error.
value :: Typeable a => Entry -> IO (Maybe a)
value (Entry _ _ _  (Just ioref)) = do
  v <- readIORef ioref
  case cast v of
    Just a  -> return (Just a)
    Nothing -> error "internal cast error"
value (Entry _ _ _  Nothing) = return Nothing

-- | Get an entry from the store or fail if no such entry exists.
getEntry :: ORef a -> Own Entry
getEntry (ORef i) = get >>= maybe err return . lookup i . snd
  where err = error ("entry not found: " ++ show i)

-- | Set the flags and value for an entry in the store.
--
-- This will take a value and place it in an IORef inside the Entry.
setEntry :: Typeable a => ORef a -> Bool -> Bool -> Maybe (IORef a) -> Own ()
setEntry (ORef i) r w n = do
  thrId <- liftIO $ myThreadId
  modifyStore (insert i (Entry r w thrId n))

-- | Modify the current store.
modifyStore :: (Store -> Store) -> Own ()
modifyStore f = modify (\(n,s) -> (n, f s))

-- | Adjust an entry in current store
--
-- This is similiar to modifyStore but takes an (Entry -> Entry) function
adjustEntry :: ORef a -> (Entry -> Entry) -> Own ()
adjustEntry (ORef i) k = modifyStore (adjust k i)

-- | Get the current flag for reading an ORef.
getReadFlag :: ORef a -> Own Bool
getReadFlag oref = fmap readFlag (getEntry oref)

-- | Get the current flag for writing to an ORef.
getWriteFlag :: ORef a -> Own Bool
getWriteFlag oref = fmap writeFlag (getEntry oref)

-- | Set the read flag for an ORef.
setReadFlag :: ORef a -> Bool -> Own ()
setReadFlag oref b = adjustEntry oref (setEntryReadFlag b)

-- | Set the write flag for an ORef.
setWriteFlag :: ORef a -> Bool -> Own ()
setWriteFlag oref b = adjustEntry oref (setEntryWriteFlag b)

-- | Get the current value of an ORef.
--
-- getValue will return a Left error string if the value in the entry is Nothing.
getValue :: Typeable a => ORef a -> Own a
getValue oref = do
  e <- getEntry oref
  v <- liftIO (value e)
  case v of
    Just a -> return a
    Nothing -> lift $ left "Cannot retrieve the value of an empty ORef"
-- TODO is it practical to return the failure left case of the Own monad in this
-- function?
-- Should Nothing be returned when the value is Nothing/empty?
-- Does it make sense to get the value of an empty ORef?

-- | Set the current value of an ORef.
--
-- This will set the value - it will __NOT__ check ownership or thread
--
-- Use the setValueEmpty function to set the value of an Entry to Nothing.
setValue :: Typeable a => ORef a -> a -> Own ()
setValue oref a = do
    r <- getReadFlag oref
    w <- getWriteFlag oref
    v <- liftIO $ newIORef a
    setEntry oref r w (Just v)

-- | Set the current value of an ORef to the empty Nothing case.
--
-- This will set the value - it will __NOT__ check ownership or thread
setValueEmpty :: Typeable a => ORef a -> Own ()
setValueEmpty oref = do
    r <- getReadFlag oref
    w <- getWriteFlag oref
    setEntry oref r w Nothing


-- ** Running the Ownership Monad

-- | Evaluate the Ownership monad operations within the context of an existing
-- Ownership monad.
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
evalOwn :: Own a -> (ID,Store) -> IO (Either String a)
evalOwn actions startState =
  runEitherT (evalStateT actions startState)

-- | Run an action in the ownership monad and return its result.
--
-- This will run the action in an initially empty context
startOwn :: Own a -> IO (Either String a)
startOwn x = runEitherT (evalStateT x (0, empty))

-- | Evaluate a state computation with a given state and return the final state,
-- discarding the final value
execOwn :: Own a -> (ID,Store) -> IO (Either String (ID,Store))
execOwn x s = runEitherT (execStateT x s)
