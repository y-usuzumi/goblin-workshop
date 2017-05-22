module Goblin.Workshop.TaskScheduler
  where

import           Data.Traversable
import           Goblin.Workshop.Task       (AnyTask, Task)
import           Goblin.Workshop.TaskResult (TaskResult, UnifiedTaskResult)
import           Goblin.Workshop.TaskRunner (AnyTaskRunner (..),
                                             TaskRunner (..))

newtype TaskScheduler t mrunner mtask e r =
  TaskScheduler { unScheduler :: Traversable t
                              => TaskRunner mrunner mtask e r
                              -> t (Task mtask e r)
                              -> mrunner (mtask (t (TaskResult e r)))
                }

newtype AnyTaskScheduler t mrunner mtask =
  AnyTaskScheduler { unAnyTaskScheduler :: Traversable t
                                        => AnyTaskRunner mrunner mtask
                                        -> t (AnyTask mtask)
                                        -> mrunner (mtask (t UnifiedTaskResult))
                   }
