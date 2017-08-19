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
dropORef :: Typeable a => ORef a -> Own ()
dropORef oref = do
    entry <- getEntry oref
    ok <- liftIO $ checkEntry entry
    guard ok -- make sure old ORef is writable and doesn't have borrowers.
    setFlag oref False
    return ()

-- | Copy the contents of one ORef to another.
--
-- A child thread should not be able to copy from its parent.
-- As long as the threadId is the same copies of an ORef can be made.
copyORef :: Typeable a => ORef a -> Own (ORef a)
copyORef oref = do
    (new, store) <- get
    entry <- getEntry oref
    ok <- liftIO $ checkThreadId entry
    guard ok
    put (new + 1, insert new entry store)
    setFlag (ORef new) True -- TODO fix this
    return (ORef new)

-- | Move the contents of one ORef to a new ORef.
-- Remove the old ORef.
-- This will fail if the old ORef has borrowers.
moveORef :: Typeable a => ORef a -> Own (ORef a)
moveORef oldORef = do
    entry <- getEntry oldORef
    ok <- liftIO $ checkEntry entry
    guard ok -- make sure old ORef is writable and doesn't have borrowers
    new <- copyORef oldORef
    dropORef oldORef
    -- if we cared about memory we would want to garbage collect/delete instead
    -- of just dropping it from the context
    return new

-- | Move the contents of one ORef to an existing ORef.
-- This will fail if either ORefs have borrowers
moveORef' :: ORef a -> ORef a -> Own ()
moveORef' oldORef@(ORef oldORefID) newORef@(ORef newORefID) = do
    entry <- getEntry oldORef
    ok <- liftIO $ checkEntry entry
    guard ok -- make sure old ORef is writable/doesn't have borrowers
    oldORefValue <- getValue oldORef
    dropORef oldORef
    -- now check that we can write to the oref that we are moving to.
    newORefEntry <- getEntry newORef
    newORefOK <- liftIO $ checkEntry newORefEntry
    guard newORefOK -- make sure the new ORef is writable/doesn't have borrowers
    setValue newORef oldORefValue
    return ()

-- | Read an ORef and use it in the given continuation.
--
-- This is - in effect - a borrow. A temporary use of
-- something before we hand it back to the original owner.
readORef :: Typeable a => ORef a -> (a -> Own b) -> Own b
readORef oref k = do
    e <- getEntry oref
    ok <- liftIO $ checkEntry e
    guard ok
    setFlag oref False
    b <- k (value e)
    setFlag oref (flag e)
    return b

-- | Write to an ORef or fail if it is not writable.
writeORef :: Typeable a => ORef a -> a -> Own ()
writeORef oref a = do
    entry <- getEntry oref
    ok <- liftIO $ checkEntry entry
    guard ok
    setValue oref a
