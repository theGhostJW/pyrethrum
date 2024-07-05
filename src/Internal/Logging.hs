{-# LANGUAGE DeriveAnyClass #-}

module Internal.Logging where

-- TODO: Explicit exports remove old code
import BasePrelude qualified as P
import CoreUtils qualified as C
import DSL.Internal.NodeEvent qualified as AE
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Text as T (intercalate)
import Effectful.Concurrent.STM (TQueue)
import Internal.LoggingCore
import Internal.ThreadEvent qualified as TE
import PyrethrumExtras as PE (head, tail, txt, (?))
import Text.Show.Pretty (pPrint)
import UnliftIO (concurrently_, finally, newIORef)
import UnliftIO.Concurrent (ThreadId)
import UnliftIO.STM (atomically, newTChanIO, newTQueueIO, readTChan, writeTChan, writeTQueue)
import Prelude hiding (atomically, lines)

newtype ExePath = ExePath {un :: [AE.Path]} deriving (Show, Eq, Ord)

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
  { path :: ExePath,
    nodeType :: TE.NodeType
  }
  deriving (Show)

mkFailure :: l -> TE.NodeType -> SomeException -> EngineEvent l a
mkFailure loc nodeType exception = Failure {exception = C.exceptionTxt exception, ..}

data EngineEvent l a
  = StartExecution
  | Start
      { nodeType :: TE.NodeType,
        loc :: l
      }
  | End
      { nodeType :: TE.NodeType,
        loc :: l
      }
  | Failure
      { nodeType :: TE.NodeType,
        loc :: l,
        exception :: C.PException
      }
  | ParentFailure
      { loc :: l,
        nodeType :: TE.NodeType,
        failLoc :: l,
        failSuiteEvent :: TE.NodeType
      }
  | NodeEvent
      { event :: a
      }
  | EndExecution
  deriving (Show)

testLogControls :: forall l a. (Show a, Show l)=> Bool -> IO (LogControls (EngineEvent l a) (TE.ThreadEvent l a), TQueue (TE.ThreadEvent l a))
testLogControls = testLogControls' expandEvent

-- -- NodeEvent (a) a loggable event generated from within a node
-- -- EngineEvent a - marks start, end and failures in test fixtures (hooks, tests) and errors
-- -- ThreadEvent a - adds thread id and index to EngineEvent
expandEvent :: C.ThreadId -> Int -> EngineEvent l a -> TE.ThreadEvent l a
expandEvent threadId idx = \case
  StartExecution -> TE.StartExecution {threadId, idx}
  Start {..} -> TE.Start {threadId, idx, ..}
  End {..} -> TE.End {threadId, idx, ..}
  Failure {..} -> TE.Failure {threadId, idx, ..}
  ParentFailure {..} -> TE.ParentFailure {threadId, idx, ..}
  NodeEvent event -> TE.NodeEvent {threadId, idx, event}
  EndExecution -> TE.EndExecution {threadId, idx}

$(deriveToJSON defaultOptions ''ExePath)
$(deriveToJSON defaultOptions ''EngineEvent)