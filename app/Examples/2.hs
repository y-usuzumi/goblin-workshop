module Main where

import           Brick                        hiding (Result)
import           Brick.BChan
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.ProgressBar
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Writer
import           Data.List                    (foldl1)
import qualified Data.Map                     as M
import           Goblin.Workshop
import qualified Graphics.Vty                 as GV
import qualified Graphics.Vty.Input.Events
import           Text.Printf

tasks :: [UniqueTask IO]
tasks = createTasksWithIds [ (1, taskA)
                           , (2, taskB)
                           , (3, taskC)
                           , (4, taskD)
                           , (5, taskE)
                           ]
  where
    taskX talk interval totalTime s = do
      talk $ TOutput "started"
      taskX_ talk interval 0 totalTime s
    taskX_ talk interval currTime totalTime s
      | currTime >= totalTime = do
          talk $ TOutput "done"
          return $ ok $ "OK"
      | otherwise = do
          let period = floor $ interval * fromIntegral 1000000
          talk $ TProgress (min (fromIntegral 100)  (currTime * 100 / totalTime))
          threadDelay period
          taskX_ talk interval (currTime + interval) totalTime s
    taskA = TalkativeTask $ \talk -> taskX talk 0.1 10 "Task 1"
    taskB = TalkativeTask $ \talk -> taskX talk 0.1 6 "Task 2"
    taskC = TalkativeTask $ \talk -> taskX talk 0.1 10 "Task 3"
    taskD = TalkativeTask $ \talk -> taskX talk 0.1 6 "Task 4"
    taskE = TalkativeTask $ \talk -> taskX talk 0.1 2 "Task 5"

dependencies :: [(TaskId, TaskId)]
dependencies = [ (1, 3)
               , (2, 3)
               , (2, 4)
               , (3, 5)
               , (4, 5)
               ]

data AppState = AppState { taskStates :: M.Map TaskId TaskState
                         , messages   :: [String]
                         }
data TaskState = NotStarted
               | Running Double
               | Done
               | Canceled
               deriving (Eq, Show)

data TaskTalk = TaskTalkProgress Double
              | TaskTalkOutput String
              deriving (Eq, Show)

data AppEvent = Started
              | TaskStateChanged TaskId TaskState
              | TaskTalk TaskId TaskTalk
              | Terminated
              deriving (Eq, Show)

translateMessage :: WorkshopMessage -> AppEvent
translateMessage WStart = Started
translateMessage WFin = Terminated
translateMessage (WTaskStateChange tid newState) = TaskStateChanged tid $
  case newState of
    WNotStarted       -> NotStarted
    WRunning progress -> Running progress
    WDone             -> Done
    WCanceled         -> Canceled
translateMessage (WTaskTalk tid wtt) = TaskTalk tid $
  case wtt of
    WTaskTalkProgress progress -> TaskTalkProgress progress
    WTaskTalkOutput output     -> TaskTalkOutput output

drawUi :: AppState -> [Widget ()]
drawUi s = let
  (messages, taskDescriptions) = stateSummary s
  in
  (:[]) $ withBorderStyle unicode $
      borderWithLabel (str "Goblin workshop sample program 2") $
      taskDescriptions <=> hBorder <=> (str $ unlines messages)
  where
    stateSummary AppState{..} = let
      taskDescriptions = foldl1 (<=>) $ map describeState (M.assocs taskStates)
      in
      (messages, taskDescriptions)
    describeState (tid, s) = str (printf "Task %2d: " tid) <+> case s of
      NotStarted       -> makeBar (0 :: Double)
      Running progress -> makeBar progress
      Done             -> makeBar (100 :: Double)
    makeBar progress = let
      prog = realToFrac (progress / 100)
      in
      updateAttrMap ( mapAttrNames [ (runningAttr, progressIncompleteAttr)
                                   , (if prog == 1 then allDoneAttr else doneAttr, progressCompleteAttr)
                                   ]
                    ) $ flip progressBar prog $ Just $
      if | prog == 0 -> "Waiting..."
         | prog == 1 -> "Done"
         | otherwise -> printf "%.2f" progress

eventHandler :: AppState -> BrickEvent n AppEvent -> EventM () (Next AppState)
eventHandler s@AppState{..} e = case e of
  VtyEvent (GV.EvKey (GV.KChar 'q') _) -> halt s
  AppEvent Started -> continue $
    updateMessages ["Workshop started"] s
  AppEvent (TaskStateChanged tid newState) -> continue $
    s{ taskStates = M.adjust (const newState) tid taskStates
     }
  AppEvent (TaskTalk tid tt) -> continue $ case tt of
    TaskTalkProgress progress -> s{ taskStates = M.adjust (const (Running progress)) tid taskStates
                                  }
    TaskTalkOutput msg -> updateMessages [printf "Task %d says: %s" tid msg] s
  AppEvent Terminated -> continue $ updateMessages ["Workshop stopped"] s
  _            -> continue s
  where
    updateMessages :: [String] -> AppState -> AppState
    updateMessages msgs s = s{ messages = execWriter $ tell messages >> tell msgs
                             }

chanThruPasser :: WorkshopBus -> (BChan AppEvent) -> IO ()
chanThruPasser wbus bchan = forever $ do
  msg <- atomically $ readTChan wbus
  writeBChan bchan $ translateMessage msg

runningAttr :: AttrName
runningAttr = attrName "running"

doneAttr :: AttrName
doneAttr = attrName "done"

allDoneAttr :: AttrName
allDoneAttr = attrName "allDone"

main :: IO ()
main = do
  let workshop = buildWorkshop tasks dependencies
      app = App { appDraw = drawUi
                , appChooseCursor = \_ _ -> Nothing
                , appHandleEvent = eventHandler
                , appStartEvent = return
                , appAttrMap = const $ attrMap GV.defAttr
                  [ (runningAttr, fg GV.white)
                  , (doneAttr, GV.brightBlack `on` GV.yellow)
                  , (allDoneAttr, GV.brightBlack `on` GV.brightGreen)
                  ]
                }
      taskStates = M.fromList $ take 5 $ zip [1..] $ repeat NotStarted
      initialState = AppState { taskStates, messages = [] }
  eventChan <- newBChan 100
  workshopChan <- atomically newWorkshopBus
  forkIO $ chanThruPasser workshopChan eventChan
  forkIO $ run workshop (Just workshopChan)
  finalState <- customMain
                (GV.mkVty GV.defaultConfig)
                (Just eventChan) app initialState
  return ()
