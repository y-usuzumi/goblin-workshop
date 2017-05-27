module Goblin.Workshop.Dispatcher where

import           Control.Monad
import           Goblin.Workshop.Bus.Dispatcher

newtype Dispatcher m =
  Scheduler { runDispatcher :: Monad m => DispatcherBus -> m ()
            }

defaultDispatcher :: DispatcherBus -> IO ()
defaultDispatcher bus = do
  atomically $ do
    writeTChan bus (Debug "Hello world")
  listen bus
  where
    listen bus = forever $ do
      msg <- readTChan bus
      case msg of
        Debug s -> putStrLn "GG"
