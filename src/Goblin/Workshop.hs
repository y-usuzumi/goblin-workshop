module Goblin.Workshop
  ( buildWorkshop
  , createTasks
  , createTasksWithIds
  , describeWorkshop
  , ok
  , err
  , isOk
  , isErr
  , run
  , newWorkshopBus
  , Result
  , Task (..)
  , TaskId
  , TaskMessage (..)
  , UniqueTask
  , WorkshopBus
  , WorkshopMessage (..)
  , WTaskState (..)
  , WTaskTalk (..)
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Goblin.Workshop.Dispatcher
import           Goblin.Workshop.Scheduler
import           Goblin.Workshop.Types
import           Goblin.Workshop.Util
import           Goblin.Workshop.Workshop
import           System.Log.Logger

run :: Workshop IO -> Maybe (TChan WorkshopMessage) -> IO ()
run workshop wbus = do
  atomically $ writeMaybeTChan wbus WStart
  termMVar <- newEmptyMVar
  dispatcherBus <- atomically newDispatcherBus
  schedulerBus <- atomically newSchedulerBus
  safeFork termMVar $ runScheduler defaultScheduler schedulerBus dispatcherBus wbus
  safeFork termMVar $ runDispatcher defaultDispatcher workshop dispatcherBus schedulerBus wbus
  takeMVar termMVar
  takeMVar termMVar
  where
    safeFork mvar action = forkFinally action $ \_ -> putMVar mvar ()
