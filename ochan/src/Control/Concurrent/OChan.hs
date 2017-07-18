module Control.Concurrent.OChan where

import Control.Concurrent
import Data.ORef

data Stream a = MVar (Item a)
data Item a   = Item a (Stream a)

data OChan a = OChan
  (MVar (Stream (ORef a)))
  (MVar (Stream (ORef a)))

-- data OChan a = OChan
--   (MVar (Stream (Own a)))
--   (MVar (Stream (Own a)))

