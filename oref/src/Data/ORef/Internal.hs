{- |
Module: Data.ORef.Internal

Internal types and functions
-}
module Data.ORef.Internal
  ( ORef(..)
  , Own
  , Flag(..)
  , Entry(..)
  -- , setEntryLocked
  -- , setEntryReadable
  , setEntryWritable
  , inThreadAndReadable
  , checkORef
  , getEntry
  -- , setEntry
  , setORefLocked
  , setORefReadable
  , setORefWritable
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


-- | Each ORef has a unique ID.
type ID = Int

-- | A typed reference to an owned value.
--
-- An ORef has a phantom type to ensure that the types of ORef are the same.
--
-- An ORef can be pattern matched as `(ORef i)` where the `i` is the integer
-- ID of the ORef.
--
-- The purpose of the ORef newtype is as a wrapper around resource information.
-- This type is used as a handle for the ownership monad to track resources
-- (through their ID) and what the type of the underlying resource is.
newtype ORef a = ORef {getID :: ID}

-- | The flag for entries
data Flag = Locked
          | Readable
          | Writable

-- | An Entry in the Ownership Monad
--
-- An entry in the store is
-- a flag,
-- the threadID of the owner,
-- and a value of arbitrary type.
--
-- Values in the Entry datatype are wrapped in the Maybe datatype to represent
-- empty entries.
--
data Entry =
  forall v. Typeable v => Entry Flag ThreadId (Maybe (IORef v))

-- | Store that maps IDs to entries.
type Store = IntMap Entry

-- | Ownership Monad with IO in the transformers stack
type Own a = StateT (ID,Store) (EitherT String IO) a


-- | The flag of an entry.
flag :: Entry -> Flag
flag (Entry f _ _) = f

-- | Check if the Entry is locked
locked :: Entry -> Bool
locked (Entry Locked _ _) = True
locked (Entry _ _ _) = False

-- | Check if an entry is readable
--
-- An Entry is readable if it is readable or writable
readable :: Entry -> Bool
readable (Entry Locked _ _) = False
readable (Entry _ _ _) = True

-- | Check if the entry is writable
writable :: Entry -> Bool
writable (Entry Writable _ _) = True
writable (Entry _ _ _) = False

-- | Adjust the flag of an Entry to Writable
setEntryLocked :: Entry -> Entry
setEntryLocked (Entry _ t v) = (Entry Locked t v)

-- | Adjust the flag of an Entry to Readable
setEntryReadable :: Entry -> Entry
setEntryReadable (Entry _ t v) = (Entry Readable t v)

-- | Adjust the flag of an Entry to Writable
setEntryWritable :: Entry -> Entry
setEntryWritable (Entry _ t v) = (Entry Writable t v)


-- | Check if the entry is empty
--
-- This will return True if the entry is empty.
entryIsEmpty :: Entry -> Bool
entryIsEmpty (Entry _f _thrId Nothing)  = True
entryIsEmpty (Entry _f _thrId (Just _)) = False

-- | Check if an ORef can be read and if it is in the same thread.
inThreadAndReadable :: ORef a-> Own Bool
inThreadAndReadable oref = do
  entry@(Entry _f thrId _v) <- getEntry oref
  liftIO $ do
    threadId <- myThreadId
    return $ (threadId == thrId) && readable entry

-- | Check if the ORef is able to be read and written to.
--
-- This will also check if the thread ID of the current thread matches the
-- thread specified in the ORef.
--
-- Checking the thread is done to prevent a child from using ORef's that it
-- inherited from its parent but was not explicitely given.
checkORef :: ORef a -> Own Bool
checkORef oref  = do
  entry@(Entry _f thrId _v) <- getEntry oref
  liftIO $ do
    threadId <- myThreadId
    return $ (threadId == thrId) && writable entry
    -- TODO should this also include if the ORef Entry is an empty Nothing value?

-- | The value inside the IORef of an entry casted to the expected type.
--
-- If the value of an entry is Nothing then Nothing will be returned.
--
-- This function will fail if there is a cast error.
value :: Typeable a => Entry -> IO (Maybe a)
value (Entry _ _ (Just ioref)) = do
  v <- readIORef ioref
  case cast v of
    Just a  -> return (Just a)
    Nothing -> error "internal cast error"
value (Entry _ _ Nothing) = return Nothing

-- | Get an entry from the store or fail if no such entry exists.
getEntry :: ORef a -> Own Entry
getEntry (ORef i) = get >>= maybe err return . lookup i . snd
  where err = lift $ left ("Entry for ORef not found. ORef likely does not exist in the context used. Entry id: " ++ show i)

-- | Set the flag and value for an entry in the store.
--
-- This will take a value and place it in an IORef inside the Entry.
setEntry :: Typeable a => ORef a -> Flag -> Maybe (IORef a) -> Own ()
setEntry (ORef i) f n = do
  thrId <- liftIO $ myThreadId
  modifyStore (insert i (Entry f thrId n))

-- | Modify the current store.
modifyStore :: (Store -> Store) -> Own ()
modifyStore f = modify (\(n,s) -> (n, f s))

-- | Adjust an entry in current store
--
-- This is similiar to modifyStore but takes an (Entry -> Entry) function
adjustEntry :: ORef a -> (Entry -> Entry) -> Own ()
adjustEntry (ORef i) k = modifyStore (adjust k i)

-- | Check if the current flag for an ORef's entry is locked
checkORefLocked :: ORef a -> Own Bool
checkORefLocked oref = fmap locked (getEntry oref)

-- | Check if the current flag for an ORef's entry is readable
checkORefReadable :: ORef a -> Own Bool
checkORefReadable oref = fmap readable (getEntry oref)

-- | Check if the current flag for an ORef's entry is writable
checkORefWritable :: ORef a -> Own Bool
checkORefWritable oref = fmap writable (getEntry oref)

-- | Set the flag for an ORef to locked
setORefLocked :: ORef a -> Own ()
setORefLocked oref = adjustEntry oref setEntryLocked

-- | Set the flag for an ORef to readable
setORefReadable :: ORef a -> Own ()
setORefReadable oref = adjustEntry oref setEntryReadable

-- | Set the flag for an ORef to writable.
setORefWritable :: ORef a -> Own ()
setORefWritable oref = adjustEntry oref setEntryWritable

getFlag :: ORef a -> Own Flag
getFlag oref = fmap flag (getEntry oref)

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
    f <- getFlag oref
    v <- liftIO $ newIORef a
    setEntry oref f (Just v)

-- | Set the current value of an ORef to the empty Nothing case.
--
-- This will set the value - it will __NOT__ check ownership or thread
setValueEmpty :: Typeable a => ORef a -> Own ()
setValueEmpty oref = do
    f <- getFlag oref
    setEntry oref f Nothing


-- ** Running the Ownership Monad

-- | Evaluate the Ownership monad operations within the context of an existing
-- Ownership monad.
--
-- This will use the state from the previous context.
continueOwn :: Own a -> Own (Either String a)
continueOwn x = do
  s <- get
  liftIO $ runEitherT (evalStateT x s)

-- | Run an action in the ownership monad and return its result.
--
-- Evaluate an ownership computation with the initial context passed as an
-- argument.
evalOwn :: Own a -> (ID,Store) -> IO (Either String a)
evalOwn actions startState =
  runEitherT (evalStateT actions startState)

-- | Create a child thread that uses a fresh ownership context to evaluate
-- the ownership operations in a new thread.
forkOwn :: Own a -> Own ()
forkOwn innerOps = do
  _ <- liftIO $ forkIO $ do
    -- child thread
    childResult <- startOwn innerOps
    case childResult of
      Left violation -> putStrLn $ "A child thread failed with the following: " ++ violation
      Right _ -> return ()
  return ()

-- | Run an action in the ownership monad and return its result.
--
-- This will run the action in an initially empty context
startOwn :: Own a -> IO (Either String a)
startOwn x = runEitherT (evalStateT x (0, empty))

-- | Evaluate a state computation with a given state and return the final state,
-- discarding the final value
execOwn :: Own a -> (ID,Store) -> IO (Either String (ID,Store))
execOwn x s = runEitherT (execStateT x s)
