module Goblin.Workshop.Workshop where

import           Data.Graph
import           Goblin.Workshop.Task

type TaskTable m = Table (TaskId, Task m)

data Workshop m = Workshop { graph     :: Graph
                           , taskTable :: TaskTable m
                           }
