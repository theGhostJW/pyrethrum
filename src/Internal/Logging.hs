{-# LANGUAGE DeriveAnyClass #-}

module Internal.Logging where

-- TODO: Explicit exports remove old code
import BasePrelude qualified as P
import DSL.Internal.NodeEvent qualified as AE
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Text as T (intercalate)
import Effectful.Concurrent.STM (TQueue)
import Internal.ThreadEvent qualified as TE
import PyrethrumExtras as PE (txt, (?), head, tail) 
import Text.Show.Pretty (pPrint)
import UnliftIO (finally)
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (atomically, newTChanIO, newTQueueIO, readTChan, writeTChan, writeTQueue)
import Prelude hiding (atomically, lines)



newtype ExePath = ExePath {un :: [AE.Path] }deriving (Show, Eq, Ord)

topPath :: ExePath -> Maybe AE.Path
topPath = PE.head . coerce

{-
 drop 2 paths for tests because tests are logged as .. parent / test path / test item path
      ExePath
        [ TestPath { id = 1 , title = "0.5.1.0 TestItem" }
        , SuiteElmPath { module' = "0.5.1.0" , path = "Test" }
        , SuiteElmPath { module' = "0.5.1" , path = "EachAround" }
        , SuiteElmPath { module' = "0.5" , path = "EachBefore" }
        , SuiteElmPath { module' = "0" , path = "OnceAround" }
        ]
      => parent
       ExePath
        [ SuiteElmPath { module' = "0.5.1" , path = "EachAround" }
        , SuiteElmPath { module' = "0.5" , path = "EachBefore" }
        , SuiteElmPath { module' = "0" , path = "OnceAround" }
        ]
       
  for Hooks the parent is simply the tail of the target path
      ExePath
        [ SuiteElmPath { module' = "0.5.1" , path = "EachAround" }
        , SuiteElmPath { module' = "0.5" , path = "EachBefore" }
        , SuiteElmPath { module' = "0" , path = "OnceAround" }
        ]
       => parent 
        ExePath
          [ SuiteElmPath { module' = "0.5" , path = "EachBefore" }
          , SuiteElmPath { module' = "0" , path = "OnceAround" }
          ]
-}
parentPath :: Bool -> ExePath -> Maybe ExePath
parentPath isTestPath (ExePath l) = 
  ExePath <$> parentPathList
  where
    parentPathList = isTestPath ? (PE.tail l >>= PE.tail) $ PE.tail l
 
-- TODO: hide string eg intercallate
displayExePath :: ExePath -> Text
displayExePath (ExePath l) = T.intercalate "." $ (.title) <$> reverse l

-- TODO :: will need thread id
data FailPoint = FailPoint
  { path :: ExePath
  , suiteEvent :: TE.SuiteEvent
  }
  deriving (Show)

exceptionTxt :: SomeException -> TE.PException
exceptionTxt e = TE.PException $ txt <$> P.lines (displayException e)

mkFailure :: l -> TE.SuiteEvent -> SomeException -> EngineEvent l a
mkFailure loc suiteEvent exception = Failure{exception = exceptionTxt exception, ..}

data EngineEvent l a
  = StartExecution
  | Start
      { suiteEvent :: TE.SuiteEvent
      , loc :: l
      }
  | End
      { suiteEvent :: TE.SuiteEvent
      , loc :: l
      }
  | Failure
      { suiteEvent :: TE.SuiteEvent
      , loc :: l
      , exception :: TE.PException
      }
  | ParentFailure
      { loc :: l
      , suiteEvent :: TE.SuiteEvent
      , failLoc :: l
      , failSuiteEvent :: TE.SuiteEvent
      }
  | NodeEvent
      { event :: a
      }
  | EndExecution
  deriving (Show)

-- apEvent (a) a loggable event arising from the framework at runtime
-- EngineEvent a - marks start, end and failures in test fixtures (hooks, tests) and errors
-- ThreadEvent a - adds thread id and index to EngineEvent
expandEvent :: TE.ThreadId -> Int -> EngineEvent loc apEvt -> TE.ThreadEvent loc apEvt
expandEvent threadId idx = \case
  StartExecution -> TE.StartExecution{threadId, idx}
  Start{..} -> TE.Start{threadId, idx, ..}
  End{..} -> TE.End{threadId, idx, ..}
  Failure{..} -> TE.Failure{threadId, idx, ..}
  ParentFailure{..} -> TE.ParentFailure{threadId, idx, ..}
  NodeEvent event -> TE.NodeEvent{threadId, idx, event}
  EndExecution -> TE.EndExecution{threadId, idx}

mkLogger :: (TE.ThreadEvent loc apEvt -> IO ()) -> IORef Int -> ThreadId -> EngineEvent loc apEvt -> IO ()
mkLogger sink threadCounter thrdId engEvnt = do
  tc <- readIORef threadCounter
  let nxt = succ tc
  finally (sink $ expandEvent (TE.mkThreadId thrdId) nxt engEvnt) $ writeIORef threadCounter nxt

-- TODO:: Logger should be wrapped in an except that sets non-zero exit code on failure

data LogControls loc apEvt = LogControls
  { sink :: TE.ThreadEvent loc apEvt -> IO ()
  , logWorker :: IO ()
  , stopWorker :: IO ()
  , log :: TQueue (TE.ThreadEvent loc apEvt)
  }

testLogControls :: forall loc apEvt. (Show loc, Show apEvt) => Bool -> IO (LogControls loc apEvt, TQueue (TE.ThreadEvent loc apEvt))
testLogControls wantConsole = do
  chn <- newTChanIO
  logQ <- newTQueueIO

  -- https://stackoverflow.com/questions/32040536/haskell-forkio-threads-writing-on-top-of-each-other-with-putstrln
  let logWorker :: IO ()
      logWorker =
        atomically (readTChan chn)
          >>= maybe
            (pure ())
            (\evt -> when wantConsole (pPrint evt) >> logWorker)

      stopWorker :: IO ()
      stopWorker = atomically $ writeTChan chn Nothing

      sink :: TE.ThreadEvent loc apEvt -> IO ()
      sink eventLog =
        atomically $ do
          writeTChan chn $ Just eventLog
          writeTQueue logQ eventLog

  pure (LogControls sink logWorker stopWorker logQ, logQ)

$(deriveToJSON defaultOptions ''ExePath)
$(deriveToJSON defaultOptions ''EngineEvent)