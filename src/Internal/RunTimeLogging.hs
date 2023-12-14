{-# LANGUAGE DeriveAnyClass #-}

module Internal.RunTimeLogging where

-- TODO: Explicit exports remove old code
import qualified BasePrelude as P
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import qualified Internal.ThreadEvent as TE
import PyrethrumExtras (txt)
import Text.Show.Pretty (pPrint)
import UnliftIO.Concurrent (ThreadId)
import Prelude hiding (atomically, lines)
import qualified DSL.Internal.ApEvent as AE
import Data.Text as T (intercalate)
import Effectful.Concurrent.STM (TQueue)
import UnliftIO.STM ( writeTChan, atomically, readTChan, newTChanIO, newTQueueIO,  writeTQueue, TChan )
import UnliftIO (finally)

newtype ExePath = ExePath {unExePath :: [AE.Path]} deriving (Show, Eq, Ord)

-- TODO: hide string eg intercallate
displayExePath :: ExePath -> Text
displayExePath ep =  T.intercalate "." $ (.title) <$> reverse ep.unExePath


-- TODO :: will need thread id
data FailPoint = FailPoint
  { path :: ExePath
  , eventType :: TE.EventType
  }
  deriving (Show)

exceptionTxt :: SomeException -> TE.PException
exceptionTxt e = TE.PException $ txt <$> P.lines (displayException e)

mkFailure :: l -> TE.EventType -> SomeException -> EngineEvent l a
mkFailure loc eventType exception = Failure {exception = exceptionTxt exception, ..}

data EngineEvent l a
  = StartExecution
  | Start
      { eventType :: TE.EventType
      , loc :: l
      }
  | End
      { eventType :: TE.EventType
      , loc :: l
      }
  | Failure
      {
       eventType :: TE.EventType
      , loc :: l
      ,  exception :: TE.PException
      }
  | ParentFailure
      { loc :: l
      , eventType :: TE.EventType
      , parentLoc :: l
      , parentEventType :: TE.EventType
      }
  | ApEvent
      { event :: a
      }
  | EndExecution
  deriving (Show)

-- apEvent (a) a loggable event arising from the framework at runtime
-- EngineEvent a - marks start, end and failures in test fixtures (hooks, tests) and errors
-- ThreadEvent a - adds thread id and index to EngineEvent
expandEvent :: TE.SThreadId -> Int -> EngineEvent loc apEvt -> TE.ThreadEvent loc apEvt
expandEvent threadId idx = \case
  StartExecution -> TE.StartExecution{threadId, idx}
  Start{..} -> TE.Start{threadId, idx, ..}
  End{..} -> TE.End{threadId, idx, ..}
  Failure{..} -> TE.Failure{threadId, idx, ..}
  ParentFailure{..} -> TE.ParentFailure{threadId, idx, ..}
  ApEvent event -> TE.ApEvent{threadId, idx, event}
  EndExecution -> TE.EndExecution{threadId, idx}

mkLogger :: (TE.ThreadEvent loc apEvt -> IO ()) -> IORef Int -> ThreadId -> EngineEvent loc apEvt -> IO ()
mkLogger sink threadCounter thrdId engEvnt = do
  tc <- readIORef threadCounter
  let nxt = succ tc
  finally (sink $ expandEvent (TE.SThreadId $ txt thrdId) nxt engEvnt) $ writeIORef threadCounter nxt


-- TODO:: Logger should be wrapped in an except that sets non-zero exit code on failure

data LogControls m loc apEvt = LogControls
  { sink :: TE.ThreadEvent loc apEvt -> IO ()
  , logWorker :: IO ()
  , stopWorker :: IO ()
  , log :: m (TQueue (TE.ThreadEvent loc apEvt))
  }

testLogControls :: forall loc apEvt. (Show loc, Show apEvt) => IO (LogControls Maybe loc apEvt, TChan (Maybe (TE.ThreadEvent loc apEvt)))
testLogControls = do

  chn <- newTChanIO
  logQ <- newTQueueIO

  -- https://stackoverflow.com/questions/32040536/haskell-forkio-threads-writing-on-top-of-each-other-with-putstrln
  let logWorker :: IO ()
      logWorker =
        atomically (readTChan chn)
          >>= maybe
            (pure ())
            (\evt -> pPrint evt >> logWorker)

      stopWorker :: IO ()
      stopWorker = atomically $ writeTChan chn Nothing

      sink :: TE.ThreadEvent loc apEvt -> IO ()
      sink eventLog =
        atomically $ do
          writeTChan chn $ Just eventLog
          writeTQueue logQ eventLog

  pure . LogControls sink logWorker stopWorker $ Just logQ


$(deriveToJSON defaultOptions ''ExePath)
$(deriveToJSON defaultOptions ''EngineEvent)