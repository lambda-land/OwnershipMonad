module Control.Concurrent.OChan
  ( OChan
  , newOChan
  , writeOChan
  , writeOChan'
  , readOChan
  , readOChan'
  ) where

import Control.Concurrent
import Control.Monad.IO.Class

import Data.Typeable (Typeable)
import Data.ORef

-- | A OChan is a channel within the context of the ownership monad
type OChan a = Own (Chan a)

-- | This creates a new channel in the ownership monad context
-- This performs IO so it comes with all the risks of an IO action
newOChan :: OChan a
newOChan = do
  ch <- liftIO newChan
  return ch

-- | Write the contents of an ORef to a channel
--
-- This will consume the ORef and it will not be able
-- to be used in the former Ownership context.
writeOChan :: Typeable a => Chan a -> ORef a -> Own ()
writeOChan ch oref = do
  -- write the contents of the ORef to the Channel
  borrowORef oref (\v -> liftIO $ writeChan ch v)
  -- Remove the oref from the ownership context
  dropORef oref

-- | Write the contents of an ORef to an Owned channel
--
-- This will consume the ORef and it will not be able
-- to be used in the former Ownership context.
writeOChan' :: Typeable a => OChan a -> ORef a -> Own ()
writeOChan' ch oref = do
  (ch >>= (\x -> writeOChan x oref))
  -- dropORef oref

-- | Read the contents of a channel in to an Owned context
--
-- This will take ownership of the value in the channel and bring it into the
-- context of the current Ownership monad.
readOChan :: Typeable a => Chan a -> Own (ORef a)
readOChan ch = do
  v <- liftIO $ readChan ch
  newORef v

-- | Read the contents of the Owned Channel
--
-- This will take ownership of the value in the channel and bring it into the
-- context of the current Ownership monad.
readOChan' :: Typeable a => OChan a -> Own (ORef a)
readOChan' oc = do
  oc >>= readOChan
