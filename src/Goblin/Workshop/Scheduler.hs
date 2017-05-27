module Goblin.Workshop.Scheduler where

import           Goblin.Workshop.Bus.Scheduler

newtype Scheduler m =
  Scheduler { runScheduler :: Monad m => SchedulerBus -> m ()
            }
