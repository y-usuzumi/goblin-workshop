module Goblin.Workshop.Workshop where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Graph
import           Goblin.Workshop.Bus.Dispatcher as DBus
import           Goblin.Workshop.Bus.Scheduler as SBus
import           Goblin.Workshop.Bus.Task as TBus
import           Goblin.Workshop.Dispatcher
import           Goblin.Workshop.Scheduler
import           Goblin.Workshop.Task

type TaskTable m = Table (TaskId, Task m)

data Workshop m = Workshop { graph     :: Graph
                           , taskTable :: TaskTable m
                           }

run :: Workshop m -> IO ()
run _ = do
  dispatcherBus <- atomically newDispatcherBus
  schedulerBus <- atomically newSchedulerBus
  taskBus <- atomically newTaskBus
  forkIO $ runDispatcher defaultDispatcher dispatcherBus
  forkIO $ runScheduler defaultScheduler schedulerBus
  forever $ threadDelay 300000000
