module Goblin.Workshop.Util where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan

writeMaybeTChan :: Maybe (TChan a) -> a -> STM ()
writeMaybeTChan Nothing _     = return ()
writeMaybeTChan (Just chan) a = writeTChan chan a
