module Goblin.Workshop.Scheduler where

import           Control.Concurrent.STM
import           Control.Monad
import           Goblin.Workshop.Bus.Scheduler

newtype Scheduler m =
  Scheduler { runScheduler :: Monad m => SchedulerBus -> m ()
            }

defaultScheduler :: Scheduler IO
defaultScheduler = Scheduler $ \bus -> do
  putStrLn "Scheduler launched"
  listen bus
  where
    listen bus = forever $ do
      msg <- atomically $ readTChan bus
      case msg of
        Debug s   -> putStrLn s
        otherwise -> error "Not implemented yet"
