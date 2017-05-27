module Goblin.Workshop.Bus.Task where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Goblin.Workshop.Task         (TaskId)

data TaskMessage = SpawnTask TaskId
                 | KillTask TaskId
                 | Debug String

type TaskBus = TChan TaskMessage

newTaskBus :: STM TaskBus
newTaskBus = newTChan
