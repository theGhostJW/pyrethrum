{-# LANGUAGE DeriveAnyClass #-}

module Internal.Logging where

-- TODO: Explicit exports remove old code

import CoreUtils (Hz (..))
import CoreUtils qualified as C
import DSL.Internal.NodeEvent qualified as NE
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Data.Text as T (intercalate)
import Effectful.Concurrent.STM (TQueue)
import Filter (FilterResult)
import Internal.LoggingCore
import PyrethrumExtras as PE (head, tail, (?))
import Prelude hiding (atomically, lines)

type Log l a = BaseLog LogContext (Event l a)

ctx :: Log l a -> LogContext
ctx = (.logContext)

evnt :: Log l a -> Event l a
evnt = (.event)

data LogContext = MkLogContext
  { threadId :: C.ThreadId,
    idx :: Int
  }
  deriving (Show)

data HookPos = Before | After | Setup | Teardown deriving (Show, Eq, Ord)

data NodeType
  = Hook Hz HookPos
  | Test
  deriving (Show, Eq, Ord)

newtype ExePath = ExePath {un :: [NE.Path]} deriving (Show, Eq, Ord)

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

data Event l a
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
        loc :: l
      }
  | End
      { nodeType :: NodeType,
        loc :: l
      }
  | Failure
      { nodeType :: NodeType,
        loc :: l,
        exception :: C.PException
      }
  | ParentFailure
      { loc :: l,
        nodeType :: NodeType,
        failLoc :: l,
        failSuiteEvent :: NodeType
      }
  | NodeEvent
      { event :: a
      }
  | EndExecution
  deriving (Show)

testLogControls :: forall l a. (Show a, Show l) => Bool -> IO (LogControls (Event l a) (Log l a), TQueue (Log l a))
testLogControls = testLogControls' expandEvent

-- -- NodeEvent (a) a loggable event generated from within a node
-- -- EngineEvent a - marks start, end and failures in test fixtures (hooks, tests) and errors
-- -- Log a - adds thread id and index to EngineEvent
expandEvent :: C.ThreadId -> Int -> Event l a -> Log l a
expandEvent threadId idx = MkLog (MkLogContext threadId idx)

$(deriveToJSON defaultOptions ''ExePath)
$(deriveJSON defaultOptions ''HookPos)
$(deriveJSON defaultOptions ''NodeType)
$(deriveToJSON defaultOptions ''Event)