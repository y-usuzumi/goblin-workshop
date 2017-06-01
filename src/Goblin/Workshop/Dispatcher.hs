module Goblin.Workshop.Dispatcher where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.List
import           Goblin.Workshop.Bus.Dispatcher as D
import           Goblin.Workshop.Bus.Scheduler  as S
import           Goblin.Workshop.Task           as T
import           Goblin.Workshop.Workshop
import           System.Log.Logger
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
  infoM "gw.d" "Dispatcher launched"
  let newAvailableTasks = getAvailableTasks workshop
  nextLoop newAvailableTasks [] workshop dbus sbus
  where
    listen workshop dbus sbus = do
      msg <- atomically $ readTChan dbus
      case msg of
        DTaskDone tid result -> do
          debugM "gw.d" $ printf "Task %d is done. Result: %s" tid (show result)
          let workshop' = removeAvailableTask tid workshop
              runningTasks = filter ((/= tid) . fst) $ getAvailableTasks workshop
              newAvailableTasks = getAvailableTasks workshop' \\ runningTasks
          nextLoop newAvailableTasks runningTasks workshop' dbus sbus
        _ -> error "Not implemented yet"
    spawnTasks tasks sbus = do
            atomically $ forM_ tasks $ \(tid, task) ->
                                         writeTChan sbus (SSpawnTask (tid, task))
    nextLoop newTasks runningTasks workshop dbus sbus = do
      if (not . null) newTasks
        then do
        spawnTasks newTasks sbus
        listen workshop dbus sbus
        else do
        if (not . null) runningTasks
          then listen workshop dbus sbus
          else do
          infoM "gw.d" "All tasks are done. Terminating scheduler"
          atomically $ writeTChan sbus SFin
          infoM "gw.d" "Bye"
