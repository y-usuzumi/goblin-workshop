module Goblin.Workshop.Scheduler where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Goblin.Workshop.Bus.Dispatcher
import           Goblin.Workshop.Bus.Scheduler
import           Goblin.Workshop.Task

newtype Scheduler m =
  Scheduler { runScheduler :: Monad m
                           => SchedulerBus m
                           -> DispatcherBus m
                           -> m ()
            }

defaultScheduler :: Scheduler IO
defaultScheduler = Scheduler $ \sbus dbus -> do
  putStrLn "Scheduler launched"
  listen sbus dbus
  where
    listen sbus dbus = do
      msg <- atomically $ readTChan sbus
      case msg of
        SSpawnTask (tid, task) -> do
          forkIO $ do
            result <- unTask task
            atomically $ writeTChan sbus $ STaskDone tid result
          listen sbus dbus
        STaskDone tid result -> do
          atomically $ writeTChan dbus $ DTaskDone tid result
          listen sbus dbus
        SFin -> return ()
        otherwise -> error "Not implemented yet"
