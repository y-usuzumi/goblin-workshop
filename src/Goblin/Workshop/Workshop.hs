module Goblin.Workshop.Workshop
  ( AnyTask (..)
  , Workshop (..)
  ) where

import           Data.Graph
import           Goblin.Workshop.Task          (AnyTask)
import           Goblin.Workshop.TaskResult    (UnifiedTaskResult)
import           Goblin.Workshop.TaskScheduler (AnyTaskScheduler)

type TaskTable m = Table (AnyTask m)

data Workshop m = Workshop { graph     :: Graph
                           , taskTable :: TaskTable m
                           }

run :: (Monad m, Traversable t)
    => Workshop mtask
    -> AnyTaskScheduler t mrunner mtask
    -> m [UnifiedTaskResult]
run Workshop{..} = undefined
