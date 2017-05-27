module Goblin.Workshop.Workshop where

import           Control.Concurrent.STM
import           Data.Graph
import           Goblin.Workshop.Dispatcher
import           Goblin.Workshop.Scheduler
import           Goblin.Workshop.Task

type TaskTable m = Table (TaskId, Task m)

data Workshop m = Workshop { graph     :: Graph
                           , taskTable :: TaskTable m
                           }

run :: Workshop m -> IO ()
run _ = do
  dispatcherBus <- atomically dispatcherBus
  schedulerBus <- atomically newSchedulerBus
  taskBus <- atomically newTaskBus
  runDispatcher dispatcherBus defaultDispatcher
  putStrLn "Hello world"
