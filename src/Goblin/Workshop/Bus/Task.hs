module Goblin.Workshop.Bus.Task where

import           Control.Concurrent.STM.TChan (TChan)
import           Goblin.Workshop.Task         (TaskId)

data Message = SpawnTask TaskId
             | KillTask TaskId

type Bus = TChan Message
