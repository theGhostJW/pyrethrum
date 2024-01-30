module DSL.Internal.ApEvent where

import Check (CheckReport)
import Data.Aeson qualified as A
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

newtype ApStateJSON = ApStateJSON {unApStateJSON :: A.Value} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''ApStateJSON)

newtype DStateJSON = DStateJSON {unDStateJSON :: A.Value} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''DStateJSON)

newtype ItemJSON = ItemJSON {unItemJSON :: A.Value} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''ItemJSON)

exceptionEvent :: SomeException -> CallStack -> ApEvent
exceptionEvent e cs =
  Framework $ Exception (toS $ displayException e) (toS $ prettyCallStack cs)

-- framework logs that represent test fixtures have a path to that fixture
-- Steps and Exceptions do not as they don't represent test fixture
data FLog
  = Action
      { path :: Path
      , item :: ItemJSON
      }
  | Parse
      { path :: Path
      , apState :: ApStateJSON
      }
  | CheckStart
      { path :: Path
      , dState :: DStateJSON
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

{-

  data LogProtocolBase e
  = FilterLog [TestFilterResult]
  | StartRun
      { runTitle :: RunTitle,
        runUtcOffsetMins :: Int,
        runConfig :: Value
      }
  | EndRun
  | StaXTGroup GroupTitle
  | EndGroup GroupTitle
  | StartHook HookType Text
  | EndHook HookType Text
  | StartTest TestLogInfo
  | EndTest Address
  | StartIteration ItemId Text Value
  | EndIteration ItemId

  | IOAction Text
  | IOAction' DetailedInfo
  | StartInteraction
  | InteractorSuccess ItemId ApStateJSON
  | InteractorFailure ItemId (FrameworkError e)
  | StartParser
  | ParserSuccess ItemId DStateJSON
  | ParserSkipped ItemId
  | ParserFailure ItemId (FrameworkError e)
  | StartChecks
  | CheckOutcome ItemId CheckReport
  | Message Text
  | Message' DetailedInfo
  | Warning Text
  | Warning' DetailedInfo
  | Error (FrameworkError e)
  deriving (Eq, Show, Functor)
\$(deriveJSON defaultOptions ''LogProtocolBase)

  -}
