module Main where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Data.Typeable

import Control.Concurrent.OChan
import Data.ORef

singleThreadedWrite :: Own ()
singleThreadedWrite = do
  -- create an ORef in the context of this thread and ownership monad
  ref <- newORef ""
  -- write to it
  writeORef ref "Quark"
  -- create a new channel
  ch <- newOChan
  -- write the oref to the channel -- this removes the oref from the context
  writeOChan ch ref
  -- TODO this operation should fail resulting in Nothing when the monad is evaluated
  writeORef ref "Odo"
  -- ^^ writing to a ref that's no longer owned

threadTask :: Typeable a => (Chan a) -> IO ()
threadTask ch = do
  ex <- (evalOwn $ do
               _ <- readOChan ch
               return ()
        )
  case ex of
    Nothing -> do putStrLn "Error"
    Just () -> do putStrLn "Success"

chanTest :: Own ()
chanTest = do
  -- create an ORef in the context of this thread and ownership monad
  ref <- newORef ""
  -- write to it
  writeORef ref "Quark"
  -- create a channel
  ch <- newOChan
  -- write the ref to the channel (removing it from this context)
  writeOChan ch ref
  -- fork the thread
  _ <- liftIO $ forkIO $ threadTask ch
  return ()

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

main :: IO ()
main = do
  example1 <- evalOwn singleThreadedWrite
  putStrLn "The example should result in Nothing"
  putStrLn $ "The example resulted in " ++ show example1
  example2 <- evalOwn chanTest
  putStrLn "The example should result in Just ()"
  putStrLn $ "The example resulted in " ++ show example2
