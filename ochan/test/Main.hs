module Main where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.Async
import Control.Monad (forever)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as S8


import Data.ORef
import Control.Concurrent.OChan

singleThreadedWrite :: Own ()
singleThreadedWrite = do
  -- create an ORef in the context of this thread and ownership monad
  ref <- newORef ""
  -- write to it
  writeORef ref "Quark"

  -- create a new channel
  let ch = newOChan
  -- write the oref to the channel -- this removes the oref from the context
  writeOChan' ch ref

  writeORef ref "Odo"
  -- ^^ writing to a ref that's no longer owned


-- threadTask :: Typeable a => (Chan a) -> IO ()
-- threadTask ch = do
--   ex <- (evalOwn $ do
--                _ <- readOChan ch
--                return ()
--         )
--   case ex of
--     Left err -> do putStrLn ("Error" ++ err)
--     Right _ -> do putStrLn "Success"


-- | A simple example using ORef's and OChan's to show a set of operations that
-- will succeed.
chanTest :: Own ()
chanTest = do
  -- create a new channel
  ch <- newOChan
  -- create an ORef in the context of this thread and ownership monad
  ref <- newORef ""
  -- write to it
  writeORef ref "Quark"
  -- write the oref to the channel (removing it from this context)
  writeOChan ch ref
  -- fork the thread
  _ <- liftIO $ forkIO $ do
    ex <- startOwn $ do
      -- we use startOwn here because we an in a new ownership context
      -- the child thread can read from the channel
      _ <- readOChan ch
      -- this places ownership of the resource in the channel within the
      -- context of the child thread
      return ()
    case ex of -- TODO make sure to get the output to the test log file
      Left err -> do putStrLn ("Error" ++ err)
      Right _ -> do putStrLn "Success"
  return ()


-- | An example of why a forked process needs to use a channel for resource
-- access instead of accessing it through the parent thread
forkedWriteExample :: Own ()
forkedWriteExample = do
  -- create an ORef in the context of this thread and Ownership context
  ref <- newORef ""
  -- fork the thread
  -- _ <- liftIO $ forkIO $ do
  forkOwn $ do
    -- child thread --
    liftIO $ putStrLn "The child thread will now try to use the ORef from its parents"
    -- The child thread will now try to run some operations on the ORef from
    -- before within the ownership monad.
    writeORef ref "test"
    -- We try to write to the ORef named ref (from the parent thread).
    -- This ORef is visable to this block of code even though it is in
    -- the child thread.
    --
    -- This will automatically result in an ownership violation and that
    -- resource will not be able to be accessed.
    --
    -- TODO show an example of how this fails without ORef's
    -- TODO add example of how forkOwn allows copies - but not moves or writes
    liftIO $ putStrLn "The child thread will have an ownership violation before\
                      \ getting to this operation"
    return ()

  -- delay parent thread to see child output
  liftIO $ threadDelay 1000

  -- create a channel
  ch <- newOChan
  -- write the oref to the channel - therefore consuming the oref
  writeOChan ch ref
  return ()





main :: IO ()
main = do
  -- example 1 --
  putStrLn "Running example 1"
  example1 <- startOwn singleThreadedWrite -- TODO example1 :: Either String ()
  putStrLn "Example 1 should result in an Error"
  putStrLn $ "Example 1 resulted in " ++ show example1

  -- example 2 --
  putStrLn "Running example 2"
  example2 <- startOwn chanTest
  -- putStrLn "The example should result in "
  putStrLn $ "Example 2 resulted in " ++ show example2

  -- example 3 --
  putStrLn "Running example 3"
  example3 <- startOwn forkedWriteExample
  putStrLn $ "Example 3 resulted in " ++ show example3
