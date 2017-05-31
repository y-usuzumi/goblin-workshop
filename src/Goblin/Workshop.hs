module Goblin.Workshop
  ( run
  , module W
  ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Goblin.Workshop.Bus.Dispatcher
import Goblin.Workshop.Bus.Scheduler
import Goblin.Workshop.Bus.Task
import Goblin.Workshop.Dispatcher
import Goblin.Workshop.Scheduler
import Goblin.Workshop.Task
import Goblin.Workshop.Workshop as W

run :: Workshop IO -> IO ()
run workshop = do
  dispatcherBus <- atomically newDispatcherBus
  schedulerBus <- atomically newSchedulerBus
  taskBus <- atomically newTaskBus
  forkIO $ runDispatcher defaultDispatcher workshop dispatcherBus schedulerBus
  forkIO $ runScheduler defaultScheduler schedulerBus
  forever $ threadDelay 300000000
