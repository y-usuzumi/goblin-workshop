module Goblin.Workshop.Types where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Dynamic
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
type UniqueTask m e o = (TaskId, Task m e o)

instance {-# OVERLAPPING #-} Eq (UniqueTask m) where
  a == b = fst a == fst b

data Task m e o where
  Task :: (TaskError e, TaskOutput o)
       => m (Result e o) -> Task m e o
  TalkativeTask :: (TaskError e, TaskOutput o)
                => ((TaskMessage e o -> m ()) -> m (Result e o)) -> Task m e o

type Result e s = Either e s

ok :: s -> Result e s
ok s = Right s

err :: e -> Result e s
err e = Left e

isOk :: Result e s -> Bool
isOk = isRight

isErr :: Result e s -> Bool
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

data DispatcherMessage m where
  DTaskDone :: (TaskError e, TaskOutput o) => TaskId -> Result e o -> DispatcherMessage m
  DTaskCanceled :: TaskId -> DispatcherMessage m
  DDebug :: String -> DispatcherMessage m

data SchedulerMessage m e o where
  SSpawnTask :: TaskId -> Task m e o -> SchedulerMessage m e o
  SKillTask :: TaskId -> SchedulerMessage m e o
  SQueryState :: TaskId -> SchedulerMessage m e o
  STaskDone :: (TaskError e, TaskOutput o) => TaskId -> Result e o -> SchedulerMessage m e o
  STaskError :: TaskId -> SchedulerMessage m e o
  STaskTalk :: TaskId -> STaskTalk e o -> SchedulerMessage m e o
  SDebug :: String -> SchedulerMessage m e o
  SFin :: SchedulerMessage m e o

class Typeable o => TaskOutput o
class Typeable e => TaskError e

data STaskTalk e o where
  STaskTalkProgress :: Progress -> STaskTalk e o
  STaskTalkOutput :: TaskOutput o => o -> STaskTalk e o
  STaskTalkError :: TaskError e => e -> STaskTalk e o

data TaskMessage e o where
  TProgress :: Progress -> TaskMessage e o
  TOutput :: o -> TaskMessage e o
  deriving (Eq, Show)

--------
-- Buses
--------

type DispatcherBus m = TChan (DispatcherMessage m)
type SchedulerBus m = TChan (SchedulerMessage m)
