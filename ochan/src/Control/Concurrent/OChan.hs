module Control.Concurrent.OChan
  ( OChan
  , newOChan
  , writeOChan
  , readOChan
  ) where

import Control.Concurrent
import Control.Monad.IO.Class

import Data.Typeable (Typeable)
import Data.ORef

-- data Stream a = MVar (Item a)
-- data Item a   = Item a (Stream a)

-- data OChan a = OChan
--   (MVar (Stream (Own a)))
--   (MVar (Stream (Own a)))

-- | A OChan is a channel within the context of the ownership monad
type OChan a = Own (Chan a)

-- | This creates a new channel in the ownership monad context
-- This performs IO so it comes with all the risks of an IO action
newOChan :: OChan a
newOChan = do
  ch <- liftIO newChan
  return ch

-- | A helper function for writeOChan
writeOChan' :: Chan a -> a -> Own ()
writeOChan' ch v = do
  _ <- liftIO $ writeChan ch v
  return ()

-- | Write the contents of ORef to a chaneel
-- This will consume the ORef and it will not be able
-- to be used in the former ownership context
writeOChan :: Typeable a => Chan a -> ORef a -> Own ()
writeOChan ch oref = do
  readORef oref (writeOChan' ch)

-- |
readOChan :: Typeable a => Chan a -> Own (ORef a)
readOChan ch = do
  v <- liftIO $ readChan ch
  (newORef v)
