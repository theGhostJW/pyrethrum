module DSL.Internal.NodeEvent where

import Check (CheckReport)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import PyrethrumExtras (toS)

-- TODO: Note plugin
{-
NodeEvent is a data type that represents events emitted by or from WITHIN a node (ie. a Hook or a Fixture) 
This is distinct from the EngineEvent data type which marks the BOUNDARIES of and events related to the nodes
themselves (such as the start and end of a Hook or skipping a Fixture)

User ULog -> ad hoc logging implemented by users of the framework
Framework FLog -> internal events from within a test or hook such as the start of a test phase such as action, parse and checks
-}

data NodeEvent
  = User ULog
  | Framework FLog
  deriving stock (Show)

newtype ItemText = ItemText {text :: Text} deriving (Eq, Show, IsString)
newtype DStateText = DStateText {text :: Text} deriving (Eq, Show, IsString)

-- framework logs that represent test fixtures have a path to that fixture
-- Steps and Exceptions do not as they don't represent test fixture
data FLog
  = Action
      { path :: Path
      , item :: ItemText
      }
  | Parse
      { path :: Path
      , apState :: ApStateText
      }
  | CheckStart
      { path :: Path
      , dState :: DStateText
      }
  | SkipedCheckStart
      { path :: Path
      }
  | Check
      { path :: Path
      , report :: CheckReport
      }
  | Step
      { message :: Text
      }
  | Step'
      { message :: Text
      , details :: Text
      }
  | Exception
      { exception :: Text
      , callStack :: Text
      }
  deriving stock (Show)

data ULog
  = StartFolder Text
  | EndFolder Text
  | Log Text
  | Log'
      { message :: Text
      , details :: Text
      }
  | Warning Text
  | Warning'
      { message :: Text
      , details :: Text
      }
  | Error Text
  | Error'
      { message :: Text
      , details :: Text
      }
  deriving stock (Eq, Show)

data Path
  = NodePath
      { module' :: Text
      , path :: Text
      }
  | TestPath
      { id :: Int
      , title :: Text
      }
  deriving (Show, Eq, Ord)


newtype ApStateText = ApStateText {text :: Text} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''ApStateText)


$(deriveJSON defaultOptions ''DStateText)
$(deriveJSON defaultOptions ''ItemText)

exceptionEvent :: Exception e => e -> CallStack -> NodeEvent
exceptionEvent e cs =
  Framework $ Exception (toS $ displayException e) (toS $ prettyCallStack cs)

$(deriveJSON defaultOptions ''ULog)
$(deriveJSON defaultOptions ''Path)
$(deriveJSON defaultOptions ''FLog)
$(deriveJSON defaultOptions ''NodeEvent)