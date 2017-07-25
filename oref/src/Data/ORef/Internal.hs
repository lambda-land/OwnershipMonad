{- |
Module: Data.ORef.Internal

Internal types and functions
-}

module Data.ORef.Internal where

import Prelude hiding (lookup)

import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Typeable (Typeable,cast)
import Data.IntMap (IntMap, empty, lookup, insert, delete)

-- | A typed reference to an owned value.
data ORef a = ORef ID

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
data Entry = forall v. Typeable v => Entry Bool v

-- | The flag of an entry.
flag :: Entry -> Bool
flag (Entry ok _) = ok

-- | The value of an entry casted to the expected type.
value :: Typeable a => Entry -> a
value (Entry _ v) = case cast v of
    Just a  -> a
    Nothing -> error "internal cast error"

-- | Modify the current store.
modifyStore :: (Store -> Store) -> Own ()
modifyStore f = modify (\(n,s) -> (n, f s))

-- | Get an entry from the store or fail if no such entry exists.
getEntry :: ID -> Own Entry
getEntry i = get >>= maybe err return . lookup i . snd
  where err = error ("entry not found: " ++ show i)

-- | Set an entry in the store.
setEntry :: Typeable a => ID -> Bool -> a -> Own ()
setEntry i ok a = modifyStore (insert i (Entry ok a))

-- | Delete an entry from the store.
deleteEntry :: ID -> Own ()
deleteEntry i = modifyStore (delete i)

-- | Get the current writeable flag for an ORef.
getFlag :: ID -> Own Bool
getFlag i = fmap flag (getEntry i)

-- | Set the current writeable flag for an ORef.
setFlag :: ID -> Bool -> Own ()
setFlag i ok = do
    Entry _ a <- getEntry i
    setEntry i ok a

-- | Get the current value of an ORef.
getValue :: Typeable a => ID -> Own a
getValue i = fmap value (getEntry i)

-- | Set the current value of an ORef.
setValue :: Typeable a => ID -> a -> Own ()
setValue i a = do
    ok <- getFlag i
    setEntry i ok a

-- | Run an action in the ownership monad and return its result.
-- evalOwn :: Own a -> Maybe a
evalOwn :: (Num t, Monad m) => StateT (t, IntMap a) (MaybeT m) a1 -> m (Maybe a1)
evalOwn x = runMaybeT (evalStateT x (0,empty))
