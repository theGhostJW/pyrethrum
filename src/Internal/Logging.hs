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

type FLog l a = FullLog LineInfo (Log l a)

{- Fully polymorphic base logging functions -}

evnt :: FullLog LineInfo (Log l a) -> Log l a
evnt = (.event)

data FullLog li evt = MkLog
  { lineInfo :: li,
    event :: evt
  }
  deriving (Show)
  deriving (Generic, NFData)

data Loggers l = MkLoggers
  { rootLogger :: l -> IO (),
    newLogger :: IO (l -> IO ())
  }

runWithLogger :: forall l. LogActions l -> (Loggers l -> IO ()) -> IO ()
runWithLogger
  MkLogActions
    { newSink,
      logWorker,
      stopWorker
    }
  action =
    do
      rootLogger <- newSink
      let loggerSource = MkLoggers rootLogger newSink
      -- logWorker and execution run concurrently
      -- logworker serialises the log events emitted by the execution
      concurrently_
        logWorker
        ( finally
            (action loggerSource)
            stopWorker
        )

-- TODO:: Logger should be wrapped in an except that sets non-zero exit code on failure

data LogActions log = MkLogActions
  { -- adds line info to the log TODO: Add timestamp (and agent?)
    newSink :: IO (log -> IO ()),
    -- worker that serializes log events
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


testLogActions' :: forall l lx. (Show lx) => ((lx -> IO ()) -> IO (l -> IO ())) -> Bool -> IO (LogActions l, STM [lx])
testLogActions' mkNewSink wantConsole = do
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

      newSink :: IO (l -> IO ())
      newSink = mkNewSink sink

  pure (MkLogActions {logWorker, stopWorker, newSink}, q2List log)

{- Logging functions specialised to Event type -}


data LineInfo = MkLineInfo
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

mkFailure :: l -> NodeType -> SomeException -> Log l a
mkFailure loc nodeType exception = Failure {exception = C.exceptionTxt exception, ..}

data Log loc nodeLog
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
      { nodeLog :: nodeLog
      }
  | EndExecution
  deriving (Show, Generic, NFData)

testLogActions :: forall l a. (Show a, Show l) => Bool -> IO (LogActions (Log l a), STM [FullLog LineInfo (Log l a)])
testLogActions = testLogActions' mkLogSinkGenerator

-- Given a base sink that will send a FullLog (including line info) into IO (), this function 
-- creates a Logger generator by intialising a new logger for the thread it is called in
-- (so the thread id, index IORef and potentially other IO properties such as agent, shard and timezone can be used) 
-- and then returns a function that will send an unexpanded Log through to IO () by adding the line info
-- and sending it to the base (FullLog) sink
mkLogSinkGenerator :: forall l a. (FullLog LineInfo (Log l a) -> IO ()) -> IO (Log l a -> IO ())
mkLogSinkGenerator fullSink = 
  logNext <$> UnliftIO.newIORef (-1) <*> P.myThreadId
  where
    addLineInfo :: C.ThreadId -> Int -> Log l a -> FullLog LineInfo (Log l a)
    addLineInfo threadId idx = MkLog (MkLineInfo threadId idx)

    logNext :: IORef Int -> ThreadId -> Log l a -> IO ()
    logNext idxRef thrdId logEvnt = do
      -- TODO: Add timestamp - need to change type of expander
      tc <- readIORef idxRef
      let nxt = succ tc
      finally (fullSink $ addLineInfo (C.mkThreadId thrdId) nxt logEvnt) $ writeIORef idxRef nxt


$(deriveToJSON defaultOptions ''ExePath)
$(deriveJSON defaultOptions ''HookPos)
$(deriveJSON defaultOptions ''NodeType)
$(deriveToJSON defaultOptions ''Log)


