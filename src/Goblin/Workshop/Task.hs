module Goblin.Workshop.Task where

import           Goblin.Workshop.Result

type TaskId = Int
newtype Task m = Task { unTask :: m Result }
