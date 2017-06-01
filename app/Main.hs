module Main where

import           Control.Concurrent
import           Control.Monad
import           GHC.IO.Handle.FD
import           Goblin.Workshop
import           System.IO
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger
import           Text.Printf

tasks :: [UniqueTask IO]
tasks = createTasksWithIds [ (1, printA)
                           , (2, printB)
                           , (3, printC)
                           , (4, printD)
                           ]
  where
    printX interval totalTime s = printX_ interval 0 totalTime s
    printX_ interval currTime totalTime s
      | currTime >= totalTime = do
          return $ ok $ printf "%s is Done!" s
      | otherwise = do
          let period = interval * 1000000
          printf "%s is working\n" s
          threadDelay period
          printX_ interval (currTime + interval) totalTime s
    printA = printX 2 10 "Task 1"
    printB = printX 3 6 "Task 2"
    printC = printX 2 10 "Task 3"
    printD = printX 2 6 "Task 4"

dependencies :: [(TaskId, TaskId)]
dependencies = [ (1, 3)
               , (2, 3)
               , (2, 4)
               ]

updateLogger :: IO ()
updateLogger = do
  h <- streamHandler stderr DEBUG
  let h' = setFormatter h (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (setHandlers [h'])
  updateGlobalLogger rootLoggerName (setLevel INFO)

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  updateLogger
  let workshop = buildWorkshop tasks dependencies
  describeWorkshop workshop
  run workshop
