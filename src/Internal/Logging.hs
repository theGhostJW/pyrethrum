{-# LANGUAGE DeriveAnyClass #-}

module Internal.Logging where

-- TODO: Explicit exports remove old code

import CoreUtils (Hz (..))
import CoreUtils qualified as C
import DSL.Internal.NodeLog qualified as NE
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Data.Text as T (intercalate)
import Filter (FilterResult)
import PyrethrumExtras as PE (head, tail, (?))
import Prelude hiding (atomically, lines)

-- TODO: Explicit exports remove old code
import BasePrelude qualified as P
import Effectful.Concurrent.STM (TQueue)
import Text.Show.Pretty (pPrint)
import UnliftIO (concurrently_, finally, newIORef, tryReadTQueue)
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (atomically, newTChanIO, newTQueueIO, readTChan, writeTChan, writeTQueue)
import Prelude hiding (atomically, lines)


{- Fully polymorphic base logging functions -}

data BaseLog lc evt = MkLog
  { logContext :: lc,
    event :: evt
  }
  deriving (Show)
  deriving (Generic, NFData)

data LoggerSource l = MkLoggerSource
  { rootLogger :: l -> IO (),
    newLogger :: IO (l -> IO ())
  }

runWithLogger :: forall l lx. LogControls l lx -> (LoggerSource l -> IO ()) -> IO ()
runWithLogger
  LogControls
    { sink,
      aggregator,
      logWorker,
      stopWorker
    }
  action =
    do
      rootLogger <- mkNewLogger
      let loggerSource = MkLoggerSource rootLogger mkNewLogger
      -- logWorker and execution run concurrently
      -- logworker serialises the log events emitted by the execution
      concurrently_
        logWorker
        ( finally
            (action loggerSource)
            stopWorker
        )
    where
      mkNewLogger :: IO (l -> IO ())
      mkNewLogger = mkLogger aggregator sink <$> UnliftIO.newIORef (-1) <*> P.myThreadId

-- adds log index and thread id to loggable event and sends it to the sink
mkLogger :: forall l lxp. (C.ThreadId -> Int -> l -> lxp) -> (lxp -> IO ()) -> IORef Int -> ThreadId -> l -> IO ()
mkLogger aggregator sink idxRef thrdId logEvnt = do
  tc <- readIORef idxRef
  let nxt = succ tc
  finally (sink $ aggregator (C.mkThreadId thrdId) nxt logEvnt) $ writeIORef idxRef nxt

-- TODO:: Logger should be wrapped in an except that sets non-zero exit code on failure

data LogControls l lx = LogControls
  { aggregator :: C.ThreadId -> Int -> l -> lx,
    sink :: lx -> IO (),
    logWorker :: IO (),
    stopWorker :: IO ()
  }

q2List :: TQueue a -> STM [a]
q2List qu = reverse <$> recurse [] qu
  where
    recurse :: [a] -> TQueue a -> STM [a]
    recurse l q =
      tryReadTQueue q
        >>= maybe (pure l) (\e -> recurse (e : l) q)

testLogControls' :: forall l lx. (Show lx) => (C.ThreadId -> Int -> l -> lx) -> Bool -> IO (LogControls l lx, STM [lx])
testLogControls' aggregator wantConsole = do
  chn <- newTChanIO
  log <- newTQueueIO

  -- https://stackoverflow.com/questions/32040536/haskell-forkio-threads-writing-on-top-of-each-other-with-putstrln
  let logWorker :: IO ()
      logWorker =
        atomically (readTChan chn)
          >>= maybe
            (pure ())
            (\evt -> when wantConsole (pPrint evt) >> logWorker)

      stopWorker :: IO ()
      stopWorker = atomically $ writeTChan chn Nothing

      sink :: lx -> IO ()
      sink eventLog =
        atomically $ do
          writeTChan chn $ Just eventLog
          writeTQueue log eventLog

  pure (LogControls {..}, q2List log)

{- Logging functions specialised to Event type -}

type Log l a = BaseLog LogContext (Event l a)

ctx :: Log l a -> LogContext
ctx = (.logContext)

evnt :: Log l a -> Event l a
evnt = (.event)

data LogContext = MkLogContext
  { threadId :: C.ThreadId,
    idx :: Int
  }
  deriving (Show, Generic, NFData)

data HookPos = Before | After | Setup | Teardown deriving (Show, Eq, Ord, Generic, NFData)

data NodeType
  = Hook Hz HookPos
  | Test
  deriving (Show, Eq, Ord, Generic, NFData)

newtype ExePath = ExePath {un :: [NE.Path]} 
 deriving (Show, Eq, Ord)
 deriving newtype NFData

topPath :: ExePath -> Maybe NE.Path
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
  { path :: ExePath,
    nodeType :: NodeType
  }
  deriving (Show)

mkFailure :: l -> NodeType -> SomeException -> Event l a
mkFailure loc nodeType exception = Failure {exception = C.exceptionTxt exception, ..}

data Event loc evnt
  = FilterLog
      { filterResuts :: [FilterResult Text]
      }
  | SuiteInitFailure
      { failure :: Text,
        notes :: Text
      }
  | StartExecution
  | Start
      { nodeType :: NodeType,
        loc :: loc
      }
  | End
      { nodeType :: NodeType,
        loc :: loc
      }
  | Failure
      { nodeType :: NodeType,
        loc :: loc,
        exception :: C.PException
      }
  | ParentFailure
      { loc :: loc,
        nodeType :: NodeType,
        failLoc :: loc,
        failSuiteEvent :: NodeType
      }
  | NodeLog
      { event :: evnt
      }
  | EndExecution
  deriving (Show, Generic, NFData)

testLogControls :: forall l a. (Show a, Show l) => Bool -> IO (LogControls (Event l a) (Log l a), STM [Log l a])
testLogControls = testLogControls' expandEvent

-- -- NodeLog (a) a loggable event generated from within a node
-- -- EngineEvent a - marks start, end and failures in test fixtures (hooks, tests) and errors
-- -- Log a - adds thread id and index to EngineEvent
expandEvent :: C.ThreadId -> Int -> Event l a -> Log l a
expandEvent threadId idx = MkLog (MkLogContext threadId idx)

$(deriveToJSON defaultOptions ''ExePath)
$(deriveJSON defaultOptions ''HookPos)
$(deriveJSON defaultOptions ''NodeType)
$(deriveToJSON defaultOptions ''Event)


