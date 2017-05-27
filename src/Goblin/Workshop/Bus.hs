module Goblin.Workshop.Bus where

import Control.Concurrent.STM.TChan
import Goblin.Workshop.Task (TaskId)

data SchedulerMessage = SpawnTask TaskId
                      | KillTask TaskId

data TaskMessage = Ter

type SchedulerBus = TChan SchedulerMessage
type TaskBus = TChan TaskMessage
