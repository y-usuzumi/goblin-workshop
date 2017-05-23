module Goblin.Workshop.TaskScheduler
  where

import           Control.Monad.State
import           Data.Map
import           Data.Traversable
import           Goblin.Workshop.Task       (AnyTask, Task, TaskId)
import           Goblin.Workshop.TaskResult (TaskResult, UnifiedTaskError,
                                             UnifiedTaskResult,
                                             UnifiedTaskSuccess (..))
import           Goblin.Workshop.TaskRunner (AnyTaskRunner (..),
                                             TaskRunner (..))

data TaskState e r = Idle
                   | Busy
                   | Done (TaskResult e r)

type TaskStateMap e r = Map TaskId (TaskState e r)
type TaskMap m e r = Map TaskId (Task m e r)

data TaskSchedulerState m e r
  = TaskSchedulerState { tasks      :: TaskMap m e r
                       , taskStates :: TaskStateMap e r
                       }

type TaskScheduler mscheduler mtask e r
  = StateT (TaskSchedulerState mtask e r) mscheduler ()

defaultIOTaskScheduler
  :: (MonadIO mscheduler, MonadIO mtask)
  => TaskScheduler mscheduler mtask e r
defaultIOTaskScheduler = do
  liftIO $ print "Hello world"
