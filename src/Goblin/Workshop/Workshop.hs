module Goblin.Workshop.Workshop where

import           Control.Arrow
import           Control.Monad
import           Data.List
import qualified Data.Map               as M
import           Goblin.Workshop.Graph
import           Goblin.Workshop.Result
import           Goblin.Workshop.Task
import           Text.Printf

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

createTasks :: Monad m => [m Result] -> [UniqueTask m]
createTasks = createTasksWithIds . zip [1..]

createTasksWithIds :: Monad m => [(TaskId, m Result)] -> [UniqueTask m]
createTasksWithIds = map (second Task)

describeWorkshop :: Workshop m -> IO ()
describeWorkshop w = do
  printf "/==========\n"
  let g = graph w
  let tasks = M.assocs $ vertices g
  let inEdges = inEdgesMap g
  printf "| Total %d tasks\n" (length tasks)
  forM_ tasks $ \(tid, _) -> do
    let deps = inEdges M.! tid
    case deps of
      [] -> printf "|   - %d\n" tid
      _ -> printf "|   - %d (dependent on %s)\n" tid (intercalate ", " (map show deps))
  printf "\\==========\n"
