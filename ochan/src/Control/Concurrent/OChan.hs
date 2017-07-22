module Control.Concurrent.OChan where

import Control.Concurrent
import Data.ORef

data Stream a = MVar (Item a)
data Item a   = Item a (Stream a)

data OChan a = OChan
  (MVar (Stream (Own a)))
  (MVar (Stream (Own a)))

newOChan :: IO (OChan a)
newOChan = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  -- return (OChan readVar writeVar)
  return undefined

writeOChan :: OChan a -> a -> IO ()
writeOChan = undefined

readOChan :: OChan a -> IO a
readOChan = undefined
