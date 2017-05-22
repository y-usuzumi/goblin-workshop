module Goblin.Workshop.Task
  ( AnyTask
  , GoblinTaskError
  , Task
  ) where

import           Goblin.Workshop.TaskResult

newtype Task m e r = Task { unTask :: m (TaskResult e r)
                          }

data AnyTask m = forall e r.
               (UnifiableTaskError r, UnifiableTaskSuccess r)
               => AnyTask { unAnyTask :: Task m e r
                          }
