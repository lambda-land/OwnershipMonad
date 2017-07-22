
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent.OChan
import Data.ORef

-- doubleWriteRefChan :: Own ()
doubleWriteRefChan = do
  -- create an ORef in the context of this thread and ownership monad
  ref <- newORef ""     -- ref :: (ORef a)
  -- write to it
  writeORef ref "Quark" -- Own ()
  liftIO $ do
    ch <- newOChan
    -- send it along the thread
    writeOChan ch ref     -- IO ()
    -- fork the thread
    forkIO $ do
      -- read from the channel - this will consume that ref
      ref' <- readOChan ch
      putStrLn "Just read an ORef from OChan"
      -- Try writing to that ref again from the parent thread
  writeORef ref "Odo"
  -- ^^ writing to a ref that's no longer owned

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented"
