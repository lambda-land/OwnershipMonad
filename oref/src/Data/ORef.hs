module Data.ORef where

import Prelude hiding (lookup)

import Control.Monad.State
import Data.Typeable (Typeable,cast)

import Data.IntMap (IntMap, empty, lookup, insert, delete)


-- ** Public Interface

-- | A typed refence to an owned value.
data ORef a = ORef ID

-- | Ownership monad.
type Own a = StateT (ID,Store) Maybe a

-- | Create a new ORef.
newORef :: Typeable a => a -> Own (ORef a)
newORef a = do
    (new,store) <- get
    put (new + 1, insert new (Entry True a) store)
    return (ORef new)

-- | Copy the contents of one ORef to another
copyORef :: ORef a -> Own (ORef a)
copyORef (ORef oldORefID) = do
  (new, store) <- get
  setFlag oldORefID False
  oldORefValue <- getValue oldORefID
  guard oldORefValue
  setFlag oldORefID True
  put (new + 1, insert new (Entry True oldORefValue) store)
  return (ORef new)

-- | Move the contents of one ORef to a new ORef
--   Remove the old ORef
moveORef :: ORef a -> Own (ORef a)
moveORef (ORef oldORefID) = do
  (new, store) <- get
  oldORefValue <- getValue oldORefID
  guard oldORefValue
  deleteEntry oldORefID
  put (new + 1, insert new (Entry True oldORefValue) store)
  return (ORef new)

-- | Move the contents of one ORef to an existing ORef
moveORef' :: ORef a -> ORef b -> Own ()
moveORef' (ORef oldID) (ORef newID) = undefined

-- | Read an ORef and use it in the given continuation.
readORef :: Typeable a => ORef a -> (a -> Own b) -> Own b
readORef (ORef i) k = do
    e <- getEntry i
    setFlag i False
    b <- k (value e)
    setFlag i (flag e)
    return b

-- | Write to an ORef or fail if it is not writeable.
writeORef :: Typeable a => ORef a -> a -> Own ()
writeORef (ORef i) a = do
    ok <- getFlag i
    guard ok
    setValue i a


-- ** Internal Only

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
evalOwn :: Own a -> Maybe a
evalOwn x = evalStateT x (0,empty)


-- ** Examples

-- | Non-conflicting write.
good :: Own (Int,Int)
good = do
    refX <- newORef 3
    refY <- newORef 5
    readORef refX (writeORef refY)
    readORef refX (\x -> readORef refY (\y -> return (x,y)))

-- | Conflicting write.
bad :: Own (Int,Int)
bad = do
    refX <- newORef 3
    refY <- newORef 5
    readORef refX (writeORef refX)
    readORef refX (\x -> readORef refY (\y -> return (x,y)))
