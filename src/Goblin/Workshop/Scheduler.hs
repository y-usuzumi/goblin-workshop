module Goblin.Workshop.Scheduler where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Goblin.Workshop.Bus.Scheduler

data Scheduler m = Scheduler { runScheduler :: Monad m => m () }

newSchedulerBus :: STM SchedulerBus
newSchedulerBus = newTChan
