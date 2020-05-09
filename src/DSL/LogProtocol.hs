module DSL.LogProtocol where

import           Common (DetailedInfo, FrameworkError)
import           Check
import           Pyrelude
import           RunElementClasses
import Data.Aeson as A
import Data.Aeson.TH

newtype RunTitle = RunTitle {unRunTitle :: Text} deriving (Eq, Show, IsString)
newtype LogTimeZone = LogTimeZone {unLogTimeZone :: TimeZone} 
    deriving (Eq, 
              Show,
              Bounded,
              Ord,
              Generic,
              ParseTime,
              FormatTime,
              Typeable
    )
newtype GroupTitle = GroupTitle {unGroupTitle :: Text} deriving (Eq, Show, IsString)
newtype TestTitle = TestTitle {unTestTitle :: Text} deriving (Eq, Show, IsString)
newtype ApStateJSON = ApStateJSON {unApStateJSON :: A.Value} deriving (Eq, Show, IsString)
newtype DStateJSON = DStateJSON {unDStateJSON :: A.Value} deriving (Eq, Show, IsString)
newtype DTestConfig = DTestConfig {unDTestConfig :: Text} deriving (Eq, Show, IsString)
newtype DRunConfig = DRunConfig {unDRunConfig :: Text} deriving (Eq, Show, IsString)
newtype WhenClause = WhenClause {unWhenClause :: Text} deriving (Eq, Show, IsString)
newtype ThenClause = ThenClause {unThenClause :: Text} deriving (Eq, Show, IsString)
data ItemId = ItemId {tstModule :: TestModule, itmId :: Int} deriving (Eq, Ord, Show)

-- needed because ItemId is used in a map
instance ToJSONKey ItemId where
  -- default implementation

instance FromJSONKey ItemId where
   -- default implementation

data ThreadInfo = ThreadInfo { 
  runId :: Text, 
  threadIndex :: Int,
  timeZone :: TimeZone
}

data LogIdxTime = LogIdxTime { 
  index :: Int,
  time :: UTCTime
}

newtype LogIndex = LogIndex { unLogIndex :: Int}

data DocActionInfo = 
    ActionInfo Text |
    ActionInfo' Text Text 
    deriving (Eq, Show)

logDoc :: DocProtocol e -> LogProtocolBase e
logDoc = IterationLog . Doc

logRun :: RunProtocol e -> LogProtocolBase e
logRun = IterationLog . Run

data DocProtocol e =   
                DocInteraction |
                DocAction DocActionInfo |
                DocIOAction Text |
                DocChecks | 
                DocCheck ItemId Text ResultExpectation GateStatus |
                
                DocMessage Text |
                DocMessage' DetailedInfo |
              
                DocWarning Text |
                DocWarning' DetailedInfo |

                DocError (FrameworkError e)
              deriving (Eq, Show, Functor)

data RunProtocol e =   
                IOAction Text |
                
                StartInteraction |
                InteractorSuccess ItemId ApStateJSON |
                InteractorFailure ItemId (FrameworkError e) |
              
                StartPrepState |
                PrepStateSuccess ItemId DStateJSON |
                PrepStateSkipped ItemId |
                PrepStateFailure ItemId (FrameworkError e) |
                
                StartChecks | 
                CheckOutcome ItemId CheckReport |

                Message Text |
                Message' DetailedInfo |
              
                Warning Text |
                Warning' DetailedInfo |

                Error (FrameworkError e)
              deriving (Eq, Show, Functor)

data SubProtocol e = 
    Doc (DocProtocol e)|
    Run (RunProtocol e)
  deriving (Eq, Show, Functor)

data BoundaryEvent = 
    FilterLog [FilterResult] |

    StartRun RunTitle Value | 
    EndRun |

    StartGroup GroupTitle |
    EndGroup GroupTitle |

    StartTest TestDisplayInfo |
    EndTest TestModule |

    StartIteration ItemId WhenClause ThenClause Value | 
    EndIteration ItemId 
  deriving (Eq, Show)

data LogProtocolBase e =
  BoundaryLog BoundaryEvent |
  IterationLog (SubProtocol e)
 deriving (Eq, Show, Functor)

type LogProtocolOut = LogProtocolBase Text

instance ToJSON LogTimeZone where
    -- this generates a Value
    toJSON (LogTimeZone TimeZone {timeZoneMinutes , timeZoneSummerOnly, timeZoneName} ) =
        object [
                "timeZoneMinutes" .= timeZoneMinutes, 
                "timeZoneSummerOnly" .= timeZoneSummerOnly,
                "timeZoneName" .= timeZoneName
                ]

    -- this encodes directly to a bytestring Builder
    toEncoding (LogTimeZone TimeZone {timeZoneMinutes , timeZoneSummerOnly, timeZoneName} )=
        pairs ("timeZoneMinutes" .= timeZoneMinutes <> 
               "timeZoneSummerOnly" .= timeZoneSummerOnly <>
                "timeZoneName" .= timeZoneName)

instance FromJSON LogTimeZone where
    parseJSON = withObject "LogTimeZone" $ \v -> LogTimeZone <$> (TimeZone
        <$> v .: "timeZoneMinutes"
        <*>  v .: "timeZoneSummerOnly"
        <*>  v .: "timeZoneName")


$(deriveJSON defaultOptions ''LogProtocolBase)
$(deriveJSON defaultOptions ''DocProtocol)
$(deriveJSON defaultOptions ''RunProtocol)
$(deriveJSON defaultOptions ''SubProtocol)
$(deriveJSON defaultOptions ''RunTitle)
$(deriveJSON defaultOptions ''GroupTitle)
$(deriveJSON defaultOptions ''TestTitle)
$(deriveJSON defaultOptions ''ApStateJSON)
$(deriveJSON defaultOptions ''DStateJSON)
$(deriveJSON defaultOptions ''ItemId)
$(deriveJSON defaultOptions ''DocActionInfo)
$(deriveJSON defaultOptions ''WhenClause)
$(deriveJSON defaultOptions ''ThenClause)
