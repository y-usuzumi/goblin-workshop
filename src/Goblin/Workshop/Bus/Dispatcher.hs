module Goblin.Workshop.Bus.Dispatcher where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Goblin.Workshop.Task         (TaskId)

data DispatcherMessage = TaskDone TaskId
                       | TaskCanceled TaskId
                       | Debug String

type DispatcherBus = TChan DispatcherMessage

newDispatcherBus :: STM DispatcherBus
newDispatcherBus = newTChan
