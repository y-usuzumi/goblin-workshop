module Goblin.Workshop.Dispatcher where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.List
import           Goblin.Workshop.Bus.Dispatcher as D
import           Goblin.Workshop.Bus.Scheduler  as S
import           Goblin.Workshop.Task           as T
import           Goblin.Workshop.Workshop
import           Text.Printf

newtype Dispatcher m =
  Dispatcher { runDispatcher :: Monad m
                             => Workshop m
                             -> DispatcherBus m
                             -> SchedulerBus m
                             -> m ()
             }

defaultDispatcher :: Dispatcher IO
defaultDispatcher = Dispatcher $ \workshop dbus sbus -> do
  putStrLn "Dispatcher launched"
  listen workshop dbus sbus
  where
    listen workshop dbus sbus = do
      msg <- atomically $ readTChan dbus
      case msg of
        DTaskDone tid result -> do
          printf "Task %d is done. Result: %s" tid (show result)
          let workshop' = removeAvailableTask tid workshop
              newAvailableTasks = getAvailableTasks workshop' \\ getAvailableTasks workshop
          when (length newAvailableTasks > 0) $ do
            atomically $ forM_ newAvailableTasks $ \(tid, task) ->
                                                     writeTChan sbus (SSpawnTask (tid, task))
            listen workshop' dbus sbus
