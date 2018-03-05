{- |
Module: Data.ORef

External API, types and functions
-}
module Data.ORef
  (
  -- * Ownership Types
    ORef
  , Own
  -- * Ownership Monad Evaluation
  , forkOwn
  , evalOwn
  , startOwn
  , execOwn
  -- * Functions to Operate on ORef's
  , newORef
  , dropORef
  , copyORef
  , moveORef
  , moveORef'
  , writeORef
  , borrowORef
  , borrowAndUpdate
  , readORef
  ) where

import Prelude hiding (lookup)

import Data.ORef.Internal

import Control.Monad.State
import Control.Monad.Trans.Either

import Data.Typeable (Typeable)
import Data.IORef (newIORef)
import Data.IntMap (insert)
import Control.Concurrent (myThreadId)

-- | Create a new ORef.
newORef :: Typeable a => a -> Own (ORef a)
newORef a = do
    (newID, store) <- get
    thrId <- liftIO $ myThreadId
    v <- liftIO $ newIORef a
    let entry = (Entry Writable thrId (Just v))
    put (newID + 1, insert newID entry store)
    return (ORef newID)

-- | Remove an ORef from the current context.
--
-- This will fail if we try to drop a ORef that we do not own. For that reason a
-- child thread does not have the ability to change oref's it can see but are
-- really owned by the parent thread.
--
-- A dropped ORef will have a flag in its entry that is set to Locked.
-- The value in the entry will be set to Nothing.
dropORef :: Typeable a => ORef a -> Own ()
dropORef oref = do
    ok <- checkORef oref  -- Check Flag
    -- TODO Should this check if the oref is already dropped (is a Nothing value)?
    -- Currently the checkORef function does not do this.
    case ok of
      False -> lift $
        left "Error during drop operation.\
             \ Make sure ORef intended to be dropped is writable and within this\
             \ thread."
      True -> do
        setORefLocked oref
        setValueEmpty oref

-- | Copy the contents of one ORef to another.
--
-- A child thread should not be able to copy from its parent.
-- As long as the threadId is the same copies of an ORef can be made.
--
-- The new entry is a copy of the old entry but with the write flag set as True.
-- We are creating a new entry which is why we do not change the older ORef.
copyORef :: ORef a -> Own (ORef a)
copyORef oref = do
    (new, store) <- get
    entry <- getEntry oref
    ok <- inThreadAndReadable oref
    case ok of
      False -> lift $ left "Error during copy operation"
      True -> do
        let newEntry = setEntryWritable entry
        put (new + 1, insert new newEntry store)
        return (ORef new)

-- | Move the contents of one ORef to a new ORef.
--
-- This will also remove the old ORef.
--
-- This will fail if the old ORef has borrowers (if either flag is set to false.)
moveORef :: Typeable a => ORef a -> Own (ORef a)
moveORef oldORef = do
    ok <- checkORef oldORef
    -- make sure old ORef is readable and writable and doesn't have borrowers
    case ok of
      False -> lift $
        left "Error during move from an oref to a new oref\
             \ check entry failed for the existing (old) oref."
      True -> do
        new <- copyORef oldORef
        dropORef oldORef
        return new

-- | Move the contents of one ORef to an existing ORef.
--
-- This will fail if either ORefs have borrowers.
moveORef' :: Typeable a => ORef a -> ORef a -> Own ()
moveORef' oORef nORef = do
    ok <- checkORef oORef
    -- makes sure old ORef is readable and writable and doesn't have borrowers
    case ok of
      False -> lift $
        left "Error during move from oref to an existing oref.\
             \ Checking the entry failed for the old oref."
      True -> do
        oldORefValue <- getValue oORef
        dropORef oORef
        -- now check that we can write to the oref that we are moving to.
        newORefOK <- checkORef nORef
        -- make sure the new ORef is writable/doesn't have borrowers
        case newORefOK of
          False -> lift $
            left "Error during move from oref to an existing oref.\
                 \ Checking the entry failed for the new oref."
          True -> setValue nORef oldORefValue

-- | Write to an ORef or fail if it is not writable.
writeORef :: Typeable a => ORef a -> a -> Own ()
writeORef oref a = do
    ok <- checkORef oref -- check if the ORef can be read and written to
    case ok of
      False -> lift $
        left "Error during write operation. Checking if the entry\
             \ could be written to or if it was in the same thread\
             \ returned False."
      True -> setValue oref a

-- | Borrow an ORef and use it in the given continuation.
--
-- A borrow is the temporary use of the value within an ORef.
--
-- We will not be mutating the ORef that is being read. For that reason a
-- borrowORef operation can have multiple functions at the same time for the same
-- ORef. This is similar to having multiple immutable borrowers.
--
-- The function is of type (a -> Own b) and it will operate by using the value
-- inside the ORef but not the ORef itself.
--
-- This means that the function that is being used in the borrow operation will
-- __not__ be able to write to the original ORef.
--
-- Even if the @writeORef@ function is partially applied on the original
-- ORef and then passed as the function given to the @borrowORef@ - it will not be
-- able to write to the original ORef during the borrow operation.
--
borrowORef :: Typeable a => ORef a -> (a -> Own b) -> Own b
borrowORef oref k = do
    ok <- checkORef oref
    case ok of
      False -> lift $
        left "Error during borrow operation.\
             \ The checks for if the entry was in the same thread\
             \ as the borrow operation and if the entry could be written to\
             \ returned false."
      True -> do
        setORefLocked oref  -- set the oref flag to locked since it is being borrowed
        v <- getValue oref
        b <- k v -- use the value in the oref
        setORefWritable oref -- Set the ORef to writable
        return b

-- | Borrow and set the value in the ORef entry.
--
-- If a function can be read and written to then it can be borrowed by a single
-- function.
-- This function will apply the function and set the value of the ORef
-- to the output of the function.
--
-- This function is provided for convenience. The same thing could be
-- achieved with a read operation and then a write operation.
--
-- ORef's store values as mutable references internally.
-- The guarantees of safety cannot be made if an ORef is given a mutable
-- value (such as an IORef or an MVar) to store.
--
-- This allows one function to have the ability to operate on the value
-- inside an ORef and mutate it.
borrowAndUpdate :: Typeable a => ORef a -> (a -> Own a) -> Own ()
borrowAndUpdate oref k = do
    ok <- checkORef oref -- check if the ORef can be read and written to
    case ok of
      False -> lift $
        left "Error during borrow and set operation. Checking if the\
             \ entry was in the same thread or if it could be\
             \ read and written to returned false."
      True -> do
        b <- borrowORef oref k
        writeORef oref b -- update the ORef

-- | Get the value out of the ORef
--
-- Reading an ORef will reveal what the contents of the ORef was
-- when the read operation was performed.
--
-- The ORef will continue to exist after this operation and the value
-- inside it will change.
--
-- In order to run a function on the current contents of an ORef please
-- use the borrowORef operations.
readORef :: Typeable a => ORef a -> Own a
readORef oref = borrowORef oref return
