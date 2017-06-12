module Goblin.Workshop.Workshop where

import           Control.Arrow
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Data.List
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           Goblin.Workshop.Graph
import           Goblin.Workshop.Types
import           Text.Printf

newWorkshopBus :: STM WorkshopBus
newWorkshopBus = newTChan

buildWorkshop :: Monad m
              => [(TaskId, Task m e o)]
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

createTasks :: Monad m => [Task m] -> [UniqueTask m]
createTasks = createTasksWithIds . zip [1..]

createTasksWithIds :: Monad m => [(TaskId, Task m)] -> [UniqueTask m]
createTasksWithIds = id

describeWorkshop :: Workshop m -> IO ()
describeWorkshop w = do
  printf "/==========\n"
  let g = graph w
  let tasks = M.assocs $ vertices g
  let inEdges = inEdgesMap g
  printf "| Total %d tasks\n" (length tasks)
  forM_ tasks $ \(tid, _) -> do
    let deps = inEdges M.! tid
    if | null deps -> printf "|   - %d\n" tid
       | otherwise -> printf "|   - %d (dependent on %s)\n" tid (intercalate ", " (map show $ S.toList deps))
  printf "\\==========\n"
