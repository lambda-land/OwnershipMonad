{-# LANGUAGE GADTs #-}

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
  , evalOwn
  ) where

import Prelude hiding (lookup)

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Concurrent

import Data.Typeable (Typeable,cast)
import Data.IntMap (IntMap, empty, lookup, insert, delete, adjust)

-- | A typed reference to an owned value.
data ORef a where
  ORef :: Typeable a => ID -> ORef a

-- | Ownership monad.
-- type Own a = StateT (ID,Store) Maybe a

-- | Ownership Monad with IO in the transformers stack
type Own a = StateT (ID,Store) (MaybeT IO) a

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

-- | This will check the flag of an entry and whether or not the thread ID of the
-- current thread matches the thread specified in the ORef. This is to prevent a
-- child from using ORef's that it inherited from its parent but was not
-- explicitely given.
checkEntry :: Entry -> IO Bool
checkEntry (Entry ok thrId _) = do
  threadId <- myThreadId
  return $ (threadId == thrId) && ok

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
setEntry :: ORef a -> Bool -> a -> Own ()
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
setValue :: ORef a -> a -> Own ()
setValue oref a = do
    ok <- getFlag oref
    setEntry oref ok a

-- | Run an action in the ownership monad and return its result.
-- evalOwn :: Own a -> Maybe a
evalOwn :: (Num t, Monad m) => StateT (t, IntMap a) (MaybeT m) a1 -> m (Maybe a1)
evalOwn x = runMaybeT (evalStateT x (0,empty))
