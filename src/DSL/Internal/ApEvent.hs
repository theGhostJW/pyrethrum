module DSL.Internal.ApEvent where

import Check (CheckReport)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import PyrethrumExtras (toS)

-- TODO: make log effect requiring out

-- TODO: make log effect requiring out
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
  = SuiteElmPath
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

newtype DStateText = DStateText {text :: Text} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''DStateText)

newtype ItemText = ItemText {text :: Text} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''ItemText)

exceptionEvent :: SomeException -> CallStack -> ApEvent
exceptionEvent e cs =
  Framework $ Exception (toS $ displayException e) (toS $ prettyCallStack cs)

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

data ApEvent
  = User ULog
  | Framework FLog
  deriving stock (Show)

$(deriveJSON defaultOptions ''ULog)
$(deriveJSON defaultOptions ''Path)
$(deriveJSON defaultOptions ''FLog)
$(deriveJSON defaultOptions ''ApEvent)