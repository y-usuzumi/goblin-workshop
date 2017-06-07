module Goblin.Workshop.Scheduler where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch
import           Goblin.Workshop.Types
import           Goblin.Workshop.Util
import           System.Log.Logger
import           Text.Printf

data SchedulerException = NoNextLoop
                        deriving (Show, Exception)

newSchedulerBus :: STM (SchedulerBus m)
newSchedulerBus = newTChan

talk :: TaskId -> SchedulerBus IO -> TaskMessage -> IO ()
talk tid sbus msg = atomically $ writeTChan sbus $ STaskTalk tid $ case msg of
  TProgress progress -> STaskTalkProgress progress
  TOutput s          -> STaskTalkOutput s

translateTalkMessage :: STaskTalk -> WTaskTalk
translateTalkMessage (STaskTalkProgress progress) = WTaskTalkProgress progress
translateTalkMessage (STaskTalkOutput s)          = WTaskTalkOutput s

defaultScheduler :: Scheduler IO
defaultScheduler = Scheduler $ \sbus dbus wbus -> do
  infoM "gw.s" "Scheduler launched"
  listen sbus dbus wbus
  where
    listen sbus dbus wbus = do
      msg <- atomically $ readTChan sbus
      _ <- case msg of
        SSpawnTask tid task -> do
          debugM "gw.s" $ printf "Spawning task: %d" tid
          void $ forkIO $ do
            result <- case task of
              Task action          -> action
              TalkativeTask action -> action (talk tid sbus)
            debugM "gw.t" $ printf "Task %d is done. Informing scheduler" tid
            atomically $ writeTChan sbus $ STaskDone tid result
        STaskDone tid result -> do
          debugM "gw.s" $ printf "Task %d is done. Informing dispatcher" tid
          void $ atomically $ writeTChan dbus $ DTaskDone tid result
        STaskTalk tid stt -> do
          void $ atomically $ writeMaybeTChan wbus $ WTaskTalk tid $ translateTalkMessage stt
        SFin -> do
          void $ infoM "gw.s" "Bye"
          throwM NoNextLoop
        _ -> error "Not implemented yet"
      listen sbus dbus wbus
      `catch` \case
        NoNextLoop -> return ()
