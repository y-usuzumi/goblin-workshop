module Goblin.Workshop.Bus.Dispatcher where

import           Control.Concurrent.STM.TChan (TChan)
import           Goblin.Workshop.Task         (TaskId)

data DispatcherMessage = TaskDone TaskId
                       | TaskCancelled TaskId

type DispatcherBus = TChan DispatcherMessage
