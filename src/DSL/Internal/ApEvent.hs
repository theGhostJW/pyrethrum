module DSL.Internal.ApEvent where

import qualified Data.Aeson as A
import Data.Aeson.TH (deriveJSON, defaultOptions)

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

newtype ApStateJSON = ApStateJSON {unApStateJSON :: A.Value} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''ApStateJSON)

newtype DStateJSON = DStateJSON {unDStateJSON :: A.Value} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''DStateJSON)

newtype ItemJSON = ItemJSON {unDStateJSON :: A.Value} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''ItemJSON)


data FLog
  = Action {item :: ItemJSON}
  | Parse {apState :: ApStateJSON}
  | CheckStart {dState :: DStateJSON} 
  | Check { description :: Text, result :: CheckResult }
  | Step Text
  | Step'
      { message :: Text
      , details :: Text
      }
  | Exception
      { exception :: SomeException
      , callStack :: CallStack
      }
  deriving stock Show

data ApEvent
  = User ULog
  | Framework FLog
  deriving stock Show



data CheckResult
  = Pass
  | Skip
  | Fail Text
  deriving (Show, Eq, Ord)




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
$(deriveJSON defaultOptions ''LogProtocolBase)
  
  -}
