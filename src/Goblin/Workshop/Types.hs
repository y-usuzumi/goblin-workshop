module Goblin.Workshop.Types where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Either
import           Goblin.Workshop.Graph
import           System.Log.Logger
import           Text.Printf

---------
-- Models
---------

type Progress = Double

-- Workshop

newtype Workshop m = Workshop { graph :: Graph TaskId (Task m)
                              }

type WorkshopBus = TChan WorkshopMessage


-- Dispatcher

newtype Dispatcher m =
  Dispatcher { runDispatcher :: Monad m
                             => Workshop m
                             -> DispatcherBus m
                             -> SchedulerBus m
                             -> Maybe WorkshopBus
                             -> m ()
             }

-- Scheduler

newtype Scheduler m =
  Scheduler { runScheduler :: Monad m
                           => SchedulerBus m
                           -> DispatcherBus m
                           -> Maybe WorkshopBus
                           -> m ()
            }

-- Task

type TaskId = Int
type UniqueTask m = (TaskId, Task m)

instance {-# OVERLAPPING #-} Eq (UniqueTask m) where
  a == b = fst a == fst b

data Task m = Task (m Result)
            | TalkativeTask ((TaskMessage -> m ()) -> m Result)

type Err = String
type Ok = String
type Result = Either Err Ok

ok :: Ok -> Result
ok s = Right s

err :: Err -> Result
err s = Left s

isOk :: Result -> Bool
isOk = isRight

isErr :: Result -> Bool
isErr = isLeft

-----------
-- Messages
-----------

data WorkshopMessage = WStart
                     | WTaskStateChange TaskId WTaskState
                     | WTaskTalk TaskId WTaskTalk
                     | WFin
                     deriving (Eq, Show)

data WTaskState = WNotStarted
                | WRunning Progress
                | WDone
                | WCanceled
                deriving (Eq, Show)

data WTaskTalk = WTaskTalkProgress Progress
               | WTaskTalkOutput String
               deriving (Eq, Show)

data DispatcherMessage m = DTaskDone TaskId Result
                         | DTaskCanceled TaskId
                         | DDebug String

data SchedulerMessage m = SSpawnTask TaskId (Task m)
                        | SKillTask TaskId
                        | SQueryState TaskId
                        | STaskDone TaskId Result
                        | STaskError TaskId
                        | STaskTalk TaskId STaskTalk
                        | SDebug String
                        | SFin

data STaskTalk = STaskTalkProgress Progress
               | STaskTalkOutput String
               deriving (Eq, Show)

data TaskMessage = TProgress Progress
                 | TOutput String
                 deriving (Eq, Show)

--------
-- Buses
--------

type DispatcherBus m = TChan (DispatcherMessage m)
type SchedulerBus m = TChan (SchedulerMessage m)
