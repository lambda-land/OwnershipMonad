{- |
Module: Data.ORef

External API, types and functions
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

-- | Remove an ORef from the current context
--
-- This will fail if we try to drop a thread that we do
-- not own. For that reason a child thread does not have
-- The ability to change oref's it can see but are owned
-- by the parent thread.
dropORef :: ORef a -> Own ()
dropORef (ORef oldORefID) = do
  entry <- getEntry oldORefID
  oldORefOK <- liftIO $ checkEntry entry
  guard oldORefOK -- make sure old ORef is writable and doesn't have borrowers. 
  setFlag oldORefID False
  return ()

-- | Copy the contents of one ORef to another.
--
-- A child thread should not be able to copy from its parent.
-- As long as the threadId is the same copies of an ORef can be made.
copyORef :: Typeable a => ORef a -> Own (ORef a)
copyORef (ORef oldORefID) = do
  (new, store) <- get
  entry <- getEntry oldORefID
  oldORefOK <- liftIO $ checkEntry entry
  put (new + 1, insert new entry store)
  setFlag new True
  return (ORef new)

-- | Move the contents of one ORef to a new ORef.
-- Remove the old ORef.
-- This will fail if the old ORef has borrowers.
moveORef :: Typeable a => ORef a -> Own (ORef a)
moveORef (ORef oldORefID) = do
  entry <- getEntry oldORefID
  ok <- liftIO $ checkEntry entry
  guard ok -- make sure old ORef is writable and doesn't have borrowers
  new <- copyORef (ORef oldORefID)
  dropORef (ORef oldORefID)
  -- if we cared about memory we would want to garbage collect/delete instead
  -- of just dropping it from the context
  return new

-- | Move the contents of one ORef to an existing ORef.
-- This will fail if either ORefs have borrowers
moveORef' :: ORef a -> ORef b -> Own ()
moveORef' (ORef oldORefID) (ORef newORefID) = do
  entry <- getEntry oldORefID
  ok <- liftIO $ checkEntry entry
  guard ok -- make sure old ORef is writable/doesn't have borrowers
  oldORefValue <- getValue oldORefID
  dropORef (ORef oldORefID)
  -- now check that we can write to the oref that we are moving to.
  newORefEntry <- getEntry newORefID
  newORefOK <- liftIO $ checkEntry newORefEntry
  guard newORefOK -- make sure the new ORef is writable/doesn't have borrowers
  setValue newORefID oldORefValue
  return ()


-- | Read an ORef and use it in the given continuation.
--
-- This is - in effect - a borrow. A temporary use of
-- something before we hand it back to the original owner.
readORef :: Typeable a => ORef a -> (a -> Own b) -> Own b
readORef (ORef i) k = do
    e <- getEntry i
    ok <- liftIO $ checkEntry e
    guard ok
    setFlag i False
    b <- k (value e)
    setFlag i (flag e)
    return b

-- | Write to an ORef or fail if it is not writable.
writeORef :: Typeable a => ORef a -> a -> Own ()
writeORef (ORef i) a = do
    entry <- getEntry i
    ok <- liftIO $ checkEntry entry
    guard ok
    setValue i a
