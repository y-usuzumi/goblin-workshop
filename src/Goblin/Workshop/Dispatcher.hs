module Goblin.Workshop.Dispatcher where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.List
import           Goblin.Workshop.Types
import           Goblin.Workshop.Util
import           Goblin.Workshop.Workshop
import           System.Log.Logger
import           Text.Printf

newDispatcherBus :: STM (DispatcherBus m)
newDispatcherBus = newTChan

defaultDispatcher :: Dispatcher IO
defaultDispatcher = Dispatcher $ \workshop dbus sbus wbus -> do
  infoM "gw.d" "Dispatcher launched"
  let newAvailableTasks = getAvailableTasks workshop
  nextLoop newAvailableTasks [] workshop dbus sbus wbus
  where
    listen workshop dbus sbus wbus = do
      msg <- atomically $ readTChan dbus
      case msg of
        DTaskDone tid result -> do
          debugM "gw.d" $ printf "Task %d is done. Result: %s" tid (show result)
          atomically $ writeMaybeTChan wbus (WTaskStateChange tid WDone)
          let workshop' = removeAvailableTask tid workshop
              runningTasks = filter ((/= tid) . fst) $ getAvailableTasks workshop
              newAvailableTasks = getAvailableTasks workshop' \\ runningTasks
          nextLoop newAvailableTasks runningTasks workshop' dbus sbus wbus
        _ -> error "Not implemented yet"
    spawnTasks tasks sbus wbus = do
            atomically $ forM_ tasks $ \(tid, task) -> do
              writeTChan sbus (SSpawnTask tid task)
              writeMaybeTChan wbus (WTaskStateChange tid $ WRunning 0)
    nextLoop newTasks runningTasks workshop dbus sbus wbus = do
      if (not . null) newTasks
        then do
        spawnTasks newTasks sbus wbus
        listen workshop dbus sbus wbus
        else do
        if (not . null) runningTasks
          then listen workshop dbus sbus wbus
          else do
          infoM "gw.d" "All tasks are done. Asking scheduler to terminate"
          atomically $ writeTChan sbus SFin
          atomically $ writeMaybeTChan wbus WFin
          infoM "gw.d" "Bye"
