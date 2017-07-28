{- |
Module: Data.ORef

Internal types and functions
-}

module Data.ORef
  (
    -- types
    ORef
  , Own
    -- functions
  , evalOwn
  , newORef
  , dropORef
  , copyORef
  , moveORef
  , moveORef'
  , readORef
  , writeORef
  ) where

import Prelude hiding (lookup)

import Data.ORef.Internal

import Control.Monad.State
import Data.Typeable (Typeable)
import Data.IntMap (insert)
import Control.Concurrent (myThreadId)

-- ** Public Interface

-- | Create a new ORef.
newORef :: Typeable a => a -> Own (ORef a)
newORef a = do
    (new,store) <- get
    thrId <- liftIO $ myThreadId
    put (new + 1, insert new (Entry True thrId a) store)
    return (ORef new)

-- | remove an ORef from the current context
dropORef :: ORef a -> Own ()
dropORef (ORef oldORefID) = do
  oldORefOK <- getFlag oldORefID
  guard oldORefOK -- make sure old ORef is writable/doesn't have borrowers
  setFlag oldORefID False
  return ()

-- | Copy the contents of one ORef to another
copyORef :: ORef a -> Own (ORef a)
copyORef (ORef oldORefID) = do
  (new, store) <- get
  setFlag oldORefID False
  oldORefValue <- getValue oldORefID
  guard oldORefValue
  setFlag oldORefID True
  thrId <- liftIO $ myThreadId
  put (new + 1, insert new (Entry True thrId oldORefValue) store)
  return (ORef new)

-- | Move the contents of one ORef to a new ORef
--   Remove the old ORef
--   This will fail if the old ORef has borrowers
moveORef :: ORef a -> Own (ORef a)
moveORef (ORef oldORefID) = do
  oldORefOK <- getFlag oldORefID
  guard oldORefOK -- make sure old ORef is writable/doesn't have borrowers
  oldORefValue <- getValue oldORefID
  guard oldORefValue
  new <- copyORef (ORef oldORefID)
  deleteEntry oldORefID
  return new

-- | Move the contents of one ORef to an existing ORef
--   This will fail if either ORefs have borrowers
moveORef' :: ORef a -> ORef b -> Own ()
moveORef' (ORef oldID) (ORef newID) = do
  oldORefOK <- getFlag oldID
  guard oldORefOK -- make sure old ORef is writable/doesn't have borrowers
  oldORefValue <- getValue oldID
  guard oldORefValue
  deleteEntry oldID
  newORefOK <- getFlag newID
  guard newORefOK -- make sure the new ORef is writable/doesn't have borrowers
  setValue newID oldORefValue
  return ()

-- | Read an ORef and use it in the given continuation.
readORef :: Typeable a => ORef a -> (a -> Own b) -> Own b
readORef (ORef i) k = do
    e <- getEntry i
    setFlag i False
    b <- k (value e)
    setFlag i (flag e)
    return b

-- | Write to an ORef or fail if it is not writable.
writeORef :: Typeable a => ORef a -> a -> Own ()
writeORef (ORef i) a = do
    ok <- getFlag i
    guard ok
    setValue i a

