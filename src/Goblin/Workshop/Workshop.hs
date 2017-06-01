module Goblin.Workshop.Workshop where

import           Data.Map              as M
import           Goblin.Workshop.Graph
import           Goblin.Workshop.Task

newtype Workshop m = Workshop { graph :: Graph TaskId (Task m)
                              }

buildWorkshop :: Monad m
              => [(TaskId, Task m)]
              -> [(TaskId, TaskId)]
              -> Workshop m
buildWorkshop tasks dependencies = Workshop $ initGraph tasks dependencies

findTaskByTaskId :: TaskId -> Workshop m -> Task m
findTaskByTaskId tid = (M.! tid) . vertices . graph

getAvailableTasks :: Workshop m -> [(TaskId, Task m)]
getAvailableTasks = entryPoints . graph

removeAvailableTask :: TaskId -> Workshop m -> Workshop m
removeAvailableTask tid = Workshop . removeEntryPoint tid . graph

removeAvailableTasks :: [TaskId] -> Workshop m -> Workshop m
removeAvailableTasks tids = Workshop . removeEntryPoints tids . graph
