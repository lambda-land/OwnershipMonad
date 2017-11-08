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
  , borrowORef
  , mutableBorrowORef
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
    let entry = (Entry True True 0 thrId a)
    put (new + 1, insert new entry store)
    return (ORef new)

-- | Remove an ORef from the current context
--
-- This will fail if we try to drop a ORef that we do not own. For that reason a
-- child thread does not have the ability to change oref's it can see but are
-- really owned by the parent thread.
dropORef :: ORef a -> Own ()
dropORef oref = do
    ok <- checkORef oref  -- Check Read and Write
    guard ok   -- make sure old ORef is writable and doesn't have borrowers.
    setWriteFlag oref False
    setReadFlag oref False
    return ()

-- | Copy the contents of one ORef to another.
--
-- A child thread should not be able to copy from its parent.
-- As long as the threadId is the same copies of an ORef can be made.
copyORef :: ORef a -> Own (ORef a)
copyORef oref = do
    (new, store) <- get
    entry <- getEntry oref
    ok <- liftIO $ checkEntryReadFlagThread entry -- TODO check borrower count here too?
    guard ok
    -- the new entry is a copy of the old entry but with the write flag set as True
    let newEntry = setEntryWriteFlag True entry
    put (new + 1, insert new newEntry store)
    return (ORef new)


-- | Move the contents of one ORef to a new ORef.
--
-- This will also remove the old ORef.
--
-- This will fail if the old ORef has borrowers (if either flag is set to false.)
moveORef :: ORef a -> Own (ORef a)
moveORef oldORef = do
    ok <- checkORef oldORef
    -- make sure old ORef is readable and writable and doesn't have borrowers
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
    ok <- checkORef oORef
    -- makes sure old ORef is readable and writable and doesn't have borrowers
    case ok of
      False -> lift $ left "Error during move from oref to an existing oref - \
                           \check entry failed for old oref."
      True -> do
        oldORefValue <- getValue oORef
        dropORef oORef
        -- now check that we can write to the oref that we are moving to.
        newORefOK <- checkORef nORef
        -- make sure the new ORef is writable/doesn't have borrowers
        case newORefOK of
          False -> lift $ left "Error during move from oref to an existing oref - \
                               \check entty failed for new oref."
          True -> do
            setValue nORef oldORefValue
            return ()

-- | Borrow an ORef and use it in the given continuation.
--
-- A borrow is the temporary and immutable use of the value within an ORef
-- before we hand it back to the original owner.
--
-- For a borrowORef operation we only care about if the ORef can be read.
--
-- We will not be mutating the ORef that is being read. For that reason a
-- borrowORef operation can have multiple functions at the same time for the same
-- ORef. This is similiar to having multiple immutable borrowers.
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
    e <- getEntry oref
    ok <- liftIO $ checkEntryReadFlagThread e
    -- This will check if we can read the entry and if it is in our thread
    -- We do not intend to write to it so we do not need to check that
    case ok of
      False -> lift $ left "Error during read operation - checking if the \
                           \ entry was in in the same thread or if it could be \
                           \ read returned false."
      True -> do
        setWriteFlag oref False
        incBC oref -- add a borrower
        -- set the oref write flag to false since it is being borrowed
        b <- k (value e) -- use the value in the oref
        decBC oref -- remove a borrower
        otherBorrowers <- hasBorrowers oref
        if otherBorrowers
            then do
                return b
            else do -- we are the last borrower
                setWriteFlag oref True
                -- set the oref write to true since it is not longer being borrowed (and can be written to)
                return b  -- return the result of using the function on ORef a

-- | Borrow an ORef in a mutable way.
--
-- If a function can be read and written to then it can be borrowed by a single
-- function. This function will apply the function. and the value of the ORef
-- will be updated.
--
-- This allows one function to have the ability to operate on the value
-- inside an ORef and mutate it.
mutableBorrowORef :: Typeable a => ORef a -> (a -> Own a) -> Own ()
mutableBorrowORef oref k = do
    ok <- checkORef oref -- check if the ORef can be read and written to
    case ok of
      False -> lift $ left "Error during mutable operation - checking if the \
                           \ entry was in in the same thread or if it could be \
                           \ read and written to returned false."
      True -> do
        setWriteFlag oref False
        setReadFlag oref False
        incBC oref -- add a borrower
        v <- getValue oref
        b <- k v -- use the value in the oref
        decBC oref -- remove a borrower
        setValue oref b -- update the ORef
        setWriteFlag oref True
        setReadFlag oref True
        return ()

-- | Write to an ORef or fail if it is not writable.
writeORef :: Typeable a => ORef a -> a -> Own ()
writeORef oref a = do
    ok <- checkORef oref -- check if the ORef can be read and written to
    case ok of
      False -> lift $ left "Error during write operation - checking if the entry\
                           \ could be written to or if it was in the same thread\
                           \ returned False."
      True -> setValue oref a
