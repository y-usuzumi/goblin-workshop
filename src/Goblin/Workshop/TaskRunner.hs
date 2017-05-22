module Goblin.Workshop.TaskRunner where

import           Goblin.Workshop.Task       (AnyTask, Task)
import           Goblin.Workshop.TaskResult (TaskResult, UnifiedTaskResult)

newtype TaskRunner mrunner mtask e r =
  TaskRunner { unTaskRunner :: Task mtask e r
                            -> mrunner (mtask (TaskResult e r))
             }
newtype AnyTaskRunner mrunner mtask =
  AnyTaskRunner { unAnyTaskRunner :: AnyTask mtask
                                  -> mrunner (mtask UnifiedTaskResult)
                }

