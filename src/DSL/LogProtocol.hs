module DSL.LogProtocol where

import Check
import Common (DetailedInfo, FrameworkError, HookType)
import Data.Aeson as A
import Data.Aeson.TH
import Pyrelude
import RunElementClasses

newtype RunTitle = RunTitle {unRunTitle :: Text} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''RunTitle)

-- newtype LogTimeZone = LogTimeZone {unLogTimeZone :: TimeZone}
--     deriving (Eq,
--               Show,
--               Bounded,
--               Ord,
--               Generic,
--               ParseTime,
--               FormatTime,
--               Typeable
--     )
newtype GroupTitle = GroupTitle {unGroupTitle :: Text} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''GroupTitle)

newtype TestTitle = TestTitle {unTestTitle :: Text} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''TestTitle)

newtype ApStateJSON = ApStateJSON {unApStateJSON :: A.Value} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''ApStateJSON)

newtype DStateJSON = DStateJSON {unDStateJSON :: A.Value} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''DStateJSON)

newtype DTestConfig = DTestConfig {unDTestConfig :: Text} deriving (Eq, Show, IsString)

newtype DRunConfig = DRunConfig {unDRunConfig :: Text} deriving (Eq, Show, IsString)

newtype WhenClause = WhenClause {unWhenClause :: Text} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''WhenClause)

newtype ThenClause = ThenClause {unThenClause :: Text} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''ThenClause)

data ItemId = ItemId { address :: Address, itmId :: Int} deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions ''ItemId)

-- needed because ItemId is used in a map
instance ToJSONKey ItemId

-- default implementation

instance FromJSONKey ItemId

-- default implementation

newtype LogIndex = LogIndex {unLogIndex :: Int} deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions ''LogIndex)

data LogEventInfo = LogEventInfo
  { rnId :: Text,
    threadIdx :: Int,
    time :: Time,
    idx :: LogIndex
  }
  deriving (Eq, Ord, Show)
$(deriveJSON defaultOptions ''LogEventInfo)

data ThreadInfo = ThreadInfo
  { runId :: Text,
    threadIndex :: Int,
    timeZone :: TimeZone
  }

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

data LogProtocolOut = LogProtocolOut
  { logIndex :: LogEventInfo,
    logInfo :: LogProtocolBase Text
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''LogProtocolOut)









