module Goblin.Workshop.Bus.Scheduler where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Goblin.Workshop.Result       (Result)
import           Goblin.Workshop.Task

data SchedulerMessage m = SSpawnTask (UniqueTask m)
                        | SKillTask TaskId
                        | SQueryState TaskId
                        | STaskDone TaskId Result
                        | STaskError TaskId
                        | STaskOutput TaskId String
                        | SDebug String
                        | SFin

type SchedulerBus m = TChan (SchedulerMessage m)

newSchedulerBus :: STM (SchedulerBus m)
newSchedulerBus = newTChan
