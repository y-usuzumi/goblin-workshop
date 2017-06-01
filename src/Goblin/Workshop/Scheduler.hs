module Goblin.Workshop.Scheduler where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Goblin.Workshop.Bus.Dispatcher
import           Goblin.Workshop.Bus.Scheduler
import           Goblin.Workshop.Task
import           System.Log.Logger
import           Text.Printf

newtype Scheduler m =
  Scheduler { runScheduler :: Monad m
                           => SchedulerBus m
                           -> DispatcherBus m
                           -> m ()
            }

defaultScheduler :: Scheduler IO
defaultScheduler = Scheduler $ \sbus dbus -> do
  infoM "gw.s" "Scheduler launched"
  listen sbus dbus
  where
    listen sbus dbus = do
      msg <- atomically $ readTChan sbus
      case msg of
        SSpawnTask (tid, task) -> do
          debugM "gw.s" $ printf "Spawning task: %d" tid
          forkIO $ do
            result <- unTask task
            debugM "gw.t" $ printf "Task %d is done. Informing scheduler" tid
            atomically $ writeTChan sbus $ STaskDone tid result
          listen sbus dbus
        STaskDone tid result -> do
          debugM "gw.s" $ printf "Task %d is done. Informing dispatcher" tid
          atomically $ writeTChan dbus $ DTaskDone tid result
          listen sbus dbus
        SFin -> do
          infoM "gw.s" "Bye"
        _ -> error "Not implemented yet"
