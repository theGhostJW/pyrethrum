module DSL.LogProtocol where

import Check
import Common (DetailedInfo, FrameworkError, HookCardinality)
import Data.Aeson as A
import Data.Aeson.TH
import Pyrelude
import RunElementClasses

newtype RunTitle = RunTitle {unRunTitle :: Text} deriving (Eq, Show, IsString)

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

newtype TestTitle = TestTitle {unTestTitle :: Text} deriving (Eq, Show, IsString)

newtype ApStateJSON = ApStateJSON {unApStateJSON :: A.Value} deriving (Eq, Show, IsString)

newtype DStateJSON = DStateJSON {unDStateJSON :: A.Value} deriving (Eq, Show, IsString)

newtype DTestConfig = DTestConfig {unDTestConfig :: Text} deriving (Eq, Show, IsString)

newtype DRunConfig = DRunConfig {unDRunConfig :: Text} deriving (Eq, Show, IsString)

newtype WhenClause = WhenClause {unWhenClause :: Text} deriving (Eq, Show, IsString)

newtype ThenClause = ThenClause {unThenClause :: Text} deriving (Eq, Show, IsString)

data ItemId = ItemId {tstModule :: TestAddress, itmId :: Int} deriving (Eq, Ord, Show)

-- needed because ItemId is used in a map
instance ToJSONKey ItemId

-- default implementation

instance FromJSONKey ItemId

-- default implementation

newtype LogIndex = LogIndex {unLogIndex :: Int} deriving (Eq, Ord, Show)

data LogEventInfo = LogEventInfo
  { rnId :: Text,
    threadIdx :: Int,
    time :: Time,
    idx :: LogIndex
  }
  deriving (Eq, Ord, Show)

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
  | StartGroup GroupTitle
  | EndGroup GroupTitle
  | StartHook HookCardinality Text
  | EndHook HookCardinality Text
  | StartTest TestDisplayInfo
  | EndTest TestAddress
  | StartIteration ItemId WhenClause ThenClause Value
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

data LogProtocolOut = LogProtocolOut
  { logIndex :: LogEventInfo,
    logInfo :: LogProtocolBase Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LogEventInfo)
$(deriveJSON defaultOptions ''LogIndex)
$(deriveJSON defaultOptions ''LogProtocolOut)
$(deriveJSON defaultOptions ''LogProtocolBase)
$(deriveJSON defaultOptions ''RunTitle)
$(deriveJSON defaultOptions ''GroupTitle)
$(deriveJSON defaultOptions ''TestTitle)
$(deriveJSON defaultOptions ''ApStateJSON)
$(deriveJSON defaultOptions ''DStateJSON)
$(deriveJSON defaultOptions ''ItemId)
$(deriveJSON defaultOptions ''WhenClause)
$(deriveJSON defaultOptions ''ThenClause)
