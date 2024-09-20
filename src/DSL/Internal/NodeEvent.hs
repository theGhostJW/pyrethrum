{-# LANGUAGE DeriveAnyClass #-}
module DSL.Internal.NodeEvent where

import Check (CheckReport)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import PyrethrumExtras (toS)

type LogSink = NodeEvent -> IO ()
-- TODO: Note plugin
{-
NodeEvent is a data type that represents specfic types of events loggged from WITHIN a node (ie. a Hook or a Fixture) 
eg. 
  - starting a test action
  - executing checks
  - user logs

This is distinct from the Events emited by the Suite Runtime that mark the boundaries of these actions
eg. 
  - start of test
  - end of hook
  - filter log
  - end suite execution
  
User UserLog -> ad hoc logging implemented by users of the framework
Framework FrameworkLog -> internal events from within a test or hook such as the start of a test phase such as action, parse and checks
-}

data NodeEvent
  = User UserLog
  | Framework FrameworkLog
  deriving (Show, Generic, NFData)


newtype ItemText = ItemText {text :: Text} 
  deriving (Eq, Show)
  deriving newtype (IsString, NFData)

newtype DStateText = DStateText {text :: Text} 
  deriving (Eq, Show)
  deriving  newtype (IsString, NFData)

-- framework logs that represent test fixtures have a path to that fixture
-- Steps and Exceptions do not as they don't represent test fixture
data FrameworkLog
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
  deriving (Show, Generic, NFData)

data UserLog
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
  deriving (Eq, Show, Generic, NFData)

data Path
  = NodePath
      { module' :: Text
      , path :: Text
      }
  | TestPath
      { id :: Int
      , title :: Text
      }
  deriving (Show, Eq, Ord, Generic, NFData)


newtype ApStateText = ApStateText {text :: Text} 
    deriving (Eq, Show)
    deriving newtype (IsString, NFData)

$(deriveJSON defaultOptions ''ApStateText)


$(deriveJSON defaultOptions ''DStateText)
$(deriveJSON defaultOptions ''ItemText)

exceptionEvent :: Exception e => e -> CallStack -> NodeEvent
exceptionEvent e cs =
  Framework $ Exception (toS $ displayException e) (toS $ prettyCallStack cs)

$(deriveJSON defaultOptions ''UserLog)
$(deriveJSON defaultOptions ''Path)
$(deriveJSON defaultOptions ''FrameworkLog)
$(deriveJSON defaultOptions ''NodeEvent)