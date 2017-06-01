module Goblin.Workshop
  ( run
  , module R
  , module T
  , module W
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Goblin.Workshop.Bus.Dispatcher
import           Goblin.Workshop.Bus.Scheduler
import           Goblin.Workshop.Dispatcher
import           Goblin.Workshop.Result         as R
import           Goblin.Workshop.Scheduler
import           Goblin.Workshop.Task           as T
import           Goblin.Workshop.Workshop       as W
import           System.Log.Logger

run :: Workshop IO -> IO ()
run workshop = do
  termMVar <- newEmptyMVar
  dispatcherBus <- atomically newDispatcherBus
  schedulerBus <- atomically newSchedulerBus
  safeFork termMVar $ runScheduler defaultScheduler schedulerBus dispatcherBus
  safeFork termMVar $ runDispatcher defaultDispatcher workshop dispatcherBus schedulerBus
  takeMVar termMVar
  takeMVar termMVar
  where
    safeFork mvar action = forkFinally action $ \_ -> putMVar mvar ()
