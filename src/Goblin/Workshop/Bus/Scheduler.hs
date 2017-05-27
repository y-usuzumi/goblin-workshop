module Goblin.Workshop.Bus.Scheduler where

import           Control.Concurrent.STM.TChan (TChan)
import           Goblin.Workshop.Result       (Result)
import           Goblin.Workshop.Task         (TaskId)

data SchedulerMessage = SpawnTask TaskId
                      | KillTask TaskId
                      | QueryState TaskId
                      | TaskDone TaskId Result
                      | TaskError TaskId
                      | TaskOutput TaskId String

type SchedulerBus = TChan SchedulerMessage
