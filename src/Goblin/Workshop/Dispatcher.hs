module Goblin.Workshop.Dispatcher where

import           Control.Concurrent.STM
import           Control.Monad
import           Goblin.Workshop.Bus.Dispatcher as D
import           Goblin.Workshop.Bus.Scheduler  as S
import           Goblin.Workshop.Task           as T
import           Goblin.Workshop.Workshop

newtype Dispatcher m =
  Dispatcher { runDispatcher :: Monad m => Workshop m -> DispatcherBus -> SchedulerBus -> m ()
            }

defaultDispatcher :: Dispatcher IO
defaultDispatcher = Dispatcher $ \workshop bus sBus -> do
  putStrLn "Dispatcher launched"
  listen bus
  where
    listen bus = forever $ do
      msg <- atomically $ readTChan bus
      case msg of
        D.Debug s   -> putStrLn s
        otherwise -> error "Not implemented yet"
