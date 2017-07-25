module Main where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Control.Monad.Trans
-- import Control.Monad.State
-- import Control.Monad.Trans.Maybe

-- import Control.Monad
-- import Data.Maybe

import Control.Concurrent.OChan
import Data.ORef

singleThreadedWrite :: Own ()
singleThreadedWrite = do
  -- create an ORef in the context of this thread and ownership monad
  ref <- newORef ""
  -- write to it
  writeORef ref "Quark"
  ch <- newOChan
  writeOChan ch ref
  -- TODO fixt writeOChan it does not work
  -- fork the thread
  writeORef ref "Odo"
  -- ^^ writing to a ref that's no longer owned

-- threadTask :: (Chan a) -> IO ()
-- threadTask ch = do
--   oref <- lift $ readOChan ch
--   putStrLn "Just read an ORef from OChan"

-- doubleWriteRefChan :: Own ()
-- doubleWriteRefChan = do
--   -- create an ORef in the context of this thread and ownership monad
--   ref <- newORef ""
--   -- write to it
--   writeORef ref "Quark"
--   ch <- newOChan
--   writeOChan ch ref
--   -- fork the thread
--   writeORef ref "Odo"
--   -- ^^ writing to a ref that's no longer owned
--   liftIO $ forkIO $ threadTask ch
--   -- Try writing to that ref again from the parent thread

-- Why thread id is needed in Ownership context
-- doubleWriteRefChan :: Own ()
-- doubleWriteRefChan = do
--   -- create an ORef in the context of this thread and ownership monad
--   ref <- newORef ""
--   -- write to it
--   ch <- newOChan
--   writeOChan ch ref
--   -- fork the thread
--   oref <- readOChan ch
--   forkIO $ do
--     writeORef ref "Quark"
--     putStrLn "Just read an ORef from OChan"
--   writeORef oref "Odo"

-- doubleWriteRefChan :: Own ()
-- doubleWriteRefChan = do
--   -- create an ORef in the context of this thread and ownership monad
--   ref <- newORef ""     -- ref :: (ORef a)
--   -- write to it
--   writeORef ref "Quark" -- Own ()
--   _ <- liftIO $ do
--     ch <- newOChan
--     -- send it along the channel
--     writeOChan ch ref     -- IO ()
--     -- fork the thread
--     forkIO $ do
--       -- read from the channel - this will consume that ref
--       _ <- readOChan ch
--       putStrLn "Just read an ORef from OChan"
--       -- Try writing to that ref again from the parent thread
--   writeORef ref "Odo"
--   -- ^^ writing to a ref that's no longer owned

main :: IO ()
main = do
  e <- evalOwn singleThreadedWrite
  putStrLn $ "The example resulted in " ++ show e
