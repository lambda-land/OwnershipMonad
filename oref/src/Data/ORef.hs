{- |
Module: Data.ORef

External API, types and functions
-}

module Data.ORef
  ( ORef
  , Own
    -- functions
  , forkOwn
  , evalOwn
  , startOwn
  , execOwn
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
import Control.Monad.Trans.Either

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
-- This will fail if we try to drop a ORef that we do
-- not own. For that reason a child thread does not have
-- The ability to change oref's it can see but are owned
-- by the parent thread.
dropORef :: ORef a -> Own ()
dropORef oref = do
    entry <- getEntry oref
    ok <- liftIO $ checkEntry entry -- TODO change to a different check
    guard ok -- make sure old ORef is writable and doesn't have borrowers.
    setFlag oref False -- TODO this would still mean we can read it though
    return ()

-- | Copy the contents of one ORef to another.
--
-- A child thread should not be able to copy from its parent.
-- As long as the threadId is the same copies of an ORef can be made.
copyORef :: ORef a -> Own (ORef a)
copyORef oref = do
    (new, store) <- get
    entry <- getEntry oref
    ok <- liftIO $ checkThreadId entry
    guard ok
    let newEntry = setEntryFlag True entry
    put (new + 1, insert new newEntry store)
    return (ORef new)


-- | Move the contents of one ORef to a new ORef.
--
-- Remove the old ORef.
--
-- This will fail if the old ORef has borrowers.
moveORef :: ORef a -> Own (ORef a)
moveORef oldORef = do
    entry <- getEntry oldORef
    ok <- liftIO $ checkEntry entry
    -- make sure old ORef is writable and doesn't have borrowers
    case ok of
      False -> lift $ left "Error during move from an oref to a new oref \
                           \ check entry failed for the exisiting (old) oref."
      True -> do
        new <- copyORef oldORef
        dropORef oldORef
        -- if we cared about memory we would want to garbage collect/delete instead
        -- of just dropping it from the context
        return new

-- | Move the contents of one ORef to an existing ORef.
--
-- This will fail if either ORefs have borrowers
moveORef' :: Typeable a => ORef a -> ORef a -> Own ()
moveORef' oORef nORef = do
    entry <- getEntry oORef
    ok <- liftIO $ checkEntry entry
    case ok of
      False -> lift $ left "Error during move from oref to an existing oref - \
                           \check entry failed for old oref."
      True -> do
        oldORefValue <- getValue oORef
        dropORef oORef
        -- now check that we can write to the oref that we are moving to.
        newORefEntry <- getEntry nORef
        newORefOK <- liftIO $ checkEntry newORefEntry
        -- make sure the new ORef is writable/doesn't have borrowers
        case newORefOK of
          False -> lift $ left "Error during move from oref to an existing oref - \
                               \check entty failed for new oref."
          True -> do
            setValue nORef oldORefValue
            return ()

-- | Read an ORef and use it in the given continuation.
--
-- This is - in effect - a borrow. A temporary use of
-- something before we hand it back to the original owner.
--
-- The function is of type (a -> Own b) and it will operate by using the value
-- and not the ORef itself.  This means that the function that is being used in
-- the read operation will not be able to write to the original ORef.
-- Even if the @writeORef@ function is partially applied on the original
-- ORef and then passed as the function given to the @readORef@ - it will not be
-- able to write to the original ORef during the read operation.
readORef :: Typeable a => ORef a -> (a -> Own b) -> Own b
readORef oref k = do
    e <- getEntry oref
    ok <- liftIO $ checkEntry e
    -- TODO do we need the check here?
    -- This should use a different check.
    -- This is checking if the ORef can be written to
    -- and for a readORef operation we only care about if it can be read
    -- Changing this check would allow for multiple operations to perform
    -- read operations on an ORef at the same time (multiple immutable borrows.)
    case ok of
      False -> lift $ left "Error during read operation - check entry failed."
      True -> do
        setFlag oref False
        -- set the oref to false (meaning it cannot be written to) since it is being borrowed
        b <- k (value e) -- use the value in the oref
        -- setFlag oref (flag e)
        -- this would always set it back to false right? TODO test this
        setFlag oref True
        -- set the oref to true since it is not longer being borrowed (and can be written to)
        return b -- return the result of using the function on ORef a

-- TODO add tests for readORef to show that after the read operation is complete
-- the ORef is marked as not having any borrowers (marked as True)
-- and that during the read operation is the ORef is marked as having borrowers
-- (marked as False)
-- TODO add test to show that a partially applied writeORef passed as the function
-- to a readORef will not be able to mutate the original ORef.
-- TODO add example of multiple operations performing read operations on an ORef


-- | Write to an ORef or fail if it is not writable.
writeORef :: Typeable a => ORef a -> a -> Own ()
writeORef oref a = do
    entry <- getEntry oref
    ok <- liftIO $ checkEntry entry
    case ok of
      False -> lift $ left "Error during write operation - check entry failed."
      True -> setValue oref a
