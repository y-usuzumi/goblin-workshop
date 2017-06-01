module Goblin.Workshop.Task where

import           Goblin.Workshop.Result


type TaskId = Int
type UniqueTask m = (TaskId, Task m)

instance {-# OVERLAPPING #-} Eq (UniqueTask m) where
  a == b = fst a == fst b

data Task m = Task { unTask :: m Result
                   }
