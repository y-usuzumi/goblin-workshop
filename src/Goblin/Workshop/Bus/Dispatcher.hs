module Goblin.Workshop.Bus.Dispatcher where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Goblin.Workshop.Result
import           Goblin.Workshop.Task         (TaskId)

data DispatcherMessage m = DTaskDone TaskId Result
                         | DTaskCanceled TaskId
                         | DDebug String

type DispatcherBus m = TChan (DispatcherMessage m)

newDispatcherBus :: STM (DispatcherBus m)
newDispatcherBus = newTChan
