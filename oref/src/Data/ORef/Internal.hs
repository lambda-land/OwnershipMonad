{- |
Module: Data.ORef.Internal

Internal types and functions
-}

module Data.ORef.Internal
  ( ORef(ORef)
  , Own
  , Entry(..)
  , flag
  , setEntryFlag
  , checkEntry
  , checkThreadId
  , value
  , getEntry
  -- , deleteEntry
  , getFlag
  , setFlag
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
import Data.IntMap (IntMap, empty, lookup, insert, delete, adjust)

-- | A typed reference to an owned value.
data ORef a = ORef ID

-- | Ownership monad.
-- type Own a = StateT (ID,Store) Maybe a

-- | Ownership Monad with IO in the transformers stack
type Own a = StateT (ID,Store) (EitherT String IO) a

-- | Each ORef has a unique ID.
type ID = Int

-- | Store that maps IDs to entries.
type Store = IntMap Entry

-- | An entry in the store is a boolean flag indicating whether this ORef can
--   be written to and a value of arbitrary type.
data Entry = forall v. Typeable v => Entry Bool ThreadId v

-- | The flag of an entry.
flag :: Entry -> Bool
flag (Entry ok _ _) = ok

-- | Adjust the flag of an Entry to the given flag
setEntryFlag :: Bool -> Entry -> Entry
setEntryFlag b (Entry _ t a) = (Entry b t a)

-- | This will check the flag of an entry and also whether or not the thread ID of the
-- current thread matches the thread specified in the ORef. This is to prevent a
-- child from using ORef's that it inherited from its parent but was not
-- explicitely given.
checkEntry :: Entry -> IO Bool
checkEntry (Entry ok thrId _) = do
  threadId <- myThreadId
  return $ (threadId == thrId) && ok

-- | Check ThreadId will only check whether the thread id of the ORef is the
-- same as the one listed in the Entry for that ORef.
--
-- This is to check for a corner-case where a thread is forked and then
-- the child thread wants to copy the value. In this situation we do not care about
-- what the entry says since it's okay to copy as long as you are in the same thread.
checkThreadId :: Entry -> IO Bool
checkThreadId (Entry _ thrId _) = do
  threadId <- myThreadId
  return $ (threadId == thrId)

-- | The value of an entry casted to the expected type.
value :: Typeable a => Entry -> a
value (Entry _ _ v) = case cast v of
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
setEntry :: Typeable a => ORef a -> Bool -> a -> Own ()
setEntry (ORef i) ok a = do
  thrId <- liftIO $ myThreadId
  modifyStore (insert i (Entry ok thrId a))

-- | Adjust an entry in current store
--
-- This is similiar to modifyStore
adjustEntry :: ORef a -> (Entry -> Entry) -> Own ()
adjustEntry (ORef i) k = modifyStore (adjust k i)

-- | Delete an entry from the store.
deleteEntry :: ORef a -> Own ()
deleteEntry (ORef i) = modifyStore (delete i)

-- | Get the current writeable flag for an ORef.
getFlag :: ORef a -> Own Bool
getFlag oref = fmap flag (getEntry oref)

-- | Set the Boolean Flag of an ORef in t
-- | Set the current writeable flag for an ORef.
setFlag :: ORef a -> Bool -> Own ()
setFlag oref ok = adjustEntry oref (setEntryFlag ok)

-- | Get the current value of an ORef.
getValue :: Typeable a => ORef a -> Own a
getValue oref = fmap value (getEntry oref)

-- | Set the current value of an ORef.
--
-- This will set the value - it will __not__ check ownership or thread
setValue :: Typeable a => ORef a -> a -> Own ()
setValue oref a = do
    ok <- getFlag oref
    setEntry oref ok a

-- continueOwn :: MonadState s m =>
--   StateT s (EitherT e m) a ->
--   m (Either e a)
-- continueOwn x = do
--   m <- get
--   runEitherT (evalStateT x m)
  -- runEitherT (get >>= evalStateT x)


-- TODO get rid of?
-- | Evaluate the Ownership monad operations within the context of
-- and existing Ownership monad.
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
-- | Evaluate a state computation with a given state and reuturn the final state,
-- discarding the final value
--
-- execOwn :: Monad m => Own a -> s -> m s
execOwn :: Monad m => StateT a1 (EitherT e m) a -> a1 -> m (Either e a1)
execOwn x s = runEitherT (execStateT x s)
