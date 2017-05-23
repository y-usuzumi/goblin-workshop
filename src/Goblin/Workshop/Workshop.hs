module Goblin.Workshop.Workshop
  ( AnyTask (..)
  , Workshop (..)
  ) where

import           Data.Graph
import           Goblin.Workshop.Task          (AnyTask, TaskId)
import           Goblin.Workshop.TaskResult    (UnifiedTaskResult)
import           Goblin.Workshop.TaskScheduler

type TaskTable m = Table (TaskId, AnyTask m)

data Workshop m = Workshop { graph     :: Graph
                           , taskTable :: TaskTable m
                           }
