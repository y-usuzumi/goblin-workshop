module Goblin.Workshop.Task where

import           Goblin.Workshop.TaskResult

newtype Task m e r = Task { unTask :: m (TaskResult e r)
                          }

data AnyTask m = forall e r.
               (UnifiableTaskError e, UnifiableTaskSuccess r)
               => AnyTask { unAnyTask :: Task m e r
                          }

type TaskId = Int
