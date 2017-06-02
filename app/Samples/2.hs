{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Control.Concurrent
import           Data.Map
import           Goblin.Workshop
import           Text.Printf

tasks :: [UniqueTask IO]
tasks = createTasksWithIds [ (1, printA)
                           , (2, printB)
                           , (3, printC)
                           , (4, printD)
                           , (5, printE)
                           ]
  where
    printX interval totalTime s = do
      printf "> Starting %s\n" s
      printX_ interval 0 totalTime s
    printX_ interval currTime totalTime s
      | currTime >= totalTime = do
          printf "< Stopping %s\n" s
          return $ ok $ "OK"
      | otherwise = do
          let period = floor $ interval * fromIntegral 1000000
          printf "* %s is working\n" s
          threadDelay period
          printX_ interval (currTime + interval) totalTime s
    printA = printX 2 10 "Task 1"
    printB = printX 3 6 "Task 2"
    printC = printX 2 10 "Task 3"
    printD = printX 2 6 "Task 4"
    printE = printX 0.5 2 "Task 5"

dependencies :: [(TaskId, TaskId)]
dependencies = [ (1, 3)
               , (2, 3)
               , (2, 4)
               , (3, 5)
               , (4, 5)
               ]

data AppState = AppState { taskStates :: Map TaskId TaskState }
data TaskState = NotStarted
               | Running Int
               | Done
data Event = Event

ui' :: Widget ()
ui' = withBorderStyle unicode $
      borderWithLabel (str "Goblin workshop sample program 2") $
      (center (str "Left") <+> hBorder <+> center (str "Right"))

eventHandler :: s -> BrickEvent n Event -> EventM () (Next s)
eventHandler s e = continue s

main :: IO ()
main = do
  let app = App { appDraw = \_ -> [ui']
                , appChooseCursor = \_ _ -> Nothing
                , appHandleEvent = eventHandler
                , appStartEvent = return
                , appAttrMap = undefined
                }
      taskStates = fromList $ take 5 $ zip [1..] $ repeat NotStarted
      initialState = AppState { taskStates }
  finalState <- defaultMain app initialState
  return ()
