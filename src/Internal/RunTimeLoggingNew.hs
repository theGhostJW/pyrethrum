{-# LANGUAGE DeriveAnyClass #-}

module Internal.RunTimeLoggingNew where

-- TODO: Explicit exports remove old code
import qualified BasePrelude as P
import Data.Aeson (ToJSON)
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Data.Set
import GHC.Show (show)
import qualified Internal.ThreadEvent as TE
import PyrethrumExtras (toS, txt)
import Text.Show.Pretty (pPrint)
import UnliftIO (TChan, TQueue, atomically, finally, newChan, newTChan, newTChanIO, newTQueue, newTQueueIO, readTChan, writeChan, writeTChan, writeTQueue)
import UnliftIO.Concurrent (ThreadId, myThreadId)
import Prelude hiding (atomically, lines)
import qualified DSL.Internal.ApEvent as AE
import Data.Text as T (Text, intercalate) 

newtype ExePath = ExePath {unExePath :: [AE.Path]} deriving (Show, Eq, Ord)

-- TODO: hide string eg intercallate
displayExePath :: ExePath -> Text
displayExePath ep =  T.intercalate "." $ (.title) <$> reverse ep.unExePath


data AbandonNew = AbandonNew
  { path :: ExePath
  , eventType :: TE.EventType
  }
  deriving (Show)


-- data ExeEventType
--   = OnceHook
--   | OnceHookRelease
--   | ThreadHook
--   | ThreadHookRelease
--   | TestHook
--   | TestHookRelease
--   | Test
--   deriving (Show, Eq, Ord, Enum)

-- isThreadedEvent :: TE.FrameworkEventType -> Bool
-- isThreadedEvent = not . isOnceEvent

-- isOnceEvent :: TE.FrameworkEventType -> Bool
-- isOnceEvent = \case
--   TE.OnceHook -> True
--   TE.OnceHookAfter -> True
--   TE.ThreadHook -> False
--   TE.ThreadHookAfter -> False
--   TE.FixtureOnceHook -> True
--   TE.FixtureOnceHookRelease -> True
--   TE.FixtureThreadHook -> False
--   TE.FixtureThreadHookRelease -> False
--   TE.TestHook -> False
--   TE.TestHookAfter -> False
--   TE.Group -> False
--   TE.Fixture -> False
--   TE.Test -> False

-- isGrouping :: TE.FrameworkEventType -> Bool
-- isGrouping = \case
--   TE.OnceHook -> False
--   TE.ThreadHook -> False
--   TE.TestHook -> False
--   TE.FixtureThreadHook -> False
--   TE.FixtureOnceHook -> False
--   TE.FixtureThreadHookRelease -> False
--   TE.FixtureOnceHookRelease -> False
--   TE.OnceHookAfter -> False
--   TE.ThreadHookAfter -> False
--   TE.TestHookAfter -> False
--   TE.Group -> True
--   TE.Fixture -> True
--   TE.Test -> False

-- isFixtureChild :: TE.FrameworkEventType -> Bool
-- isFixtureChild = \case
--   TE.OnceHook -> False
--   TE.ThreadHook -> False
--   TE.TestHook -> True
--   TE.FixtureThreadHook -> True
--   TE.FixtureOnceHook -> True
--   TE.FixtureThreadHookRelease -> True
--   TE.FixtureOnceHookRelease -> True
--   TE.OnceHookAfter -> False
--   TE.ThreadHookAfter -> False
--   TE.TestHookAfter -> True
--   TE.Group -> False
--   TE.Fixture -> False
--   TE.Test -> True

-- endIsTerminal :: TE.FrameworkEventType -> Bool
-- endIsTerminal = \case
--   TE.FixtureThreadHookRelease -> True
--   TE.FixtureOnceHookRelease -> True
--   TE.OnceHookAfter -> True
--   TE.ThreadHookAfter -> True
--   TE.TestHookAfter -> True
--   TE.Group -> True
--   TE.Fixture -> True
--   TE.Test -> True
--   TE.OnceHook -> False
--   TE.ThreadHook -> False
--   TE.TestHook -> False
--   TE.FixtureThreadHook -> False
--   TE.FixtureOnceHook -> False

exceptionTxt :: SomeException -> TE.PException
exceptionTxt e = TE.PException $ txt <$> P.lines (displayException e)

mkFailure :: l -> TE.EventType -> SomeException -> EngineEvent l a
mkFailure loc eventType exception = Failure {exception = exceptionTxt exception, ..}

mkParentFailure :: ExePath -> TE.EventType -> AbandonNew -> EngineEvent ExePath a
mkParentFailure l evt ab  =
  ParentFailure
    { 
      loc = l
    , eventType = evt
    , parentLoc = ab.path
    , parentEventType = ab.eventType
    }

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

testLogControls :: forall loc apEvt. (Show loc, Show apEvt) => TChan (Maybe (TE.ThreadEvent loc apEvt)) -> TQueue (TE.ThreadEvent loc apEvt) -> IO (LogControls Maybe loc apEvt)
testLogControls chn logQ = do
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