module Goblin.Workshop.Dispatcher where

import           Control.Concurrent.STM
import           Control.Monad
import           Goblin.Workshop.Bus.Dispatcher

newtype Dispatcher m =
  Dispatcher { runDispatcher :: Monad m => DispatcherBus -> m ()
            }

defaultDispatcher :: Dispatcher IO
defaultDispatcher = Dispatcher $ \bus -> do
  putStrLn "Dispatcher launched"
  listen bus
  where
    listen bus = forever $ do
      msg <- atomically $ readTChan bus
      case msg of
        Debug s -> putStrLn s
        otherwise -> error "Not implemented yet"
