
{-# LANGUAGE PolyKinds #-}

module DSL.LogProtocol where

import           Common (DetailedInfo, AppError)
import           Check
import           Pyrelude
import           RunElementClasses
import GHC.Generics
import OrphanedInstances
import Data.Aeson as A
import Data.Either
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T

newtype RunTitle = RunTitle {unRunTitle :: Text} deriving (Eq, Show, IsString)
newtype GroupTitle = GroupTitle {unGroupTitle :: Text} deriving (Eq, Show, IsString)
newtype TestTitle = TestTitle {unTestTitle :: Text} deriving (Eq, Show, IsString)
newtype ApStateDisplay = ApStateDisplay {unApStateDisplay :: Text} deriving (Eq, Show, IsString)
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

data DocActionInfo = 
    ActionInfo Text |
    ActionInfoM Text Text 
    deriving (Eq, Show)

logDoc :: DocProtocol -> LogProtocol
logDoc = IterationLog . Doc

logRun :: RunProtocol -> LogProtocol
logRun = IterationLog . Run

data DocProtocol =   
                DocInteraction |
                DocAction DocActionInfo |
                DocIOAction Text |
                DocChecks | 
                DocCheck ItemId Text ResultExpectation GateStatus |
                
                DocMessage Text |
                DocMessage' DetailedInfo |
              
                DocWarning Text |
                DocWarning' DetailedInfo |

                DocError AppError
              deriving (Eq, Show)

data RunProtocol =   
                IOAction Text |
                StartPrepState |
                StartInteraction |
                InteractorSuccess ItemId ApStateDisplay |
                InteractorFailure ItemId AppError |
              
                PrepStateSuccess ItemId DStateJSON |
                PrepStateFailure ItemId AppError |
                StartChecks | 
                CheckOutcome ItemId CheckReport |

                Message Text |
                Message' DetailedInfo |
              
                Warning Text |
                Warning' DetailedInfo |

                Error AppError
              deriving (Eq, Show)

data SubProtocol = 
    Doc DocProtocol |
    Run RunProtocol
  deriving (Eq, Show)

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

data LogProtocol =
  BoundaryLog BoundaryEvent |
  IterationLog SubProtocol
 deriving (Eq, Show)

$(deriveJSON defaultOptions ''LogProtocol)
$(deriveJSON defaultOptions ''DocProtocol)
$(deriveJSON defaultOptions ''RunProtocol)
$(deriveJSON defaultOptions ''SubProtocol)
$(deriveJSON defaultOptions ''RunTitle)
$(deriveJSON defaultOptions ''GroupTitle)
$(deriveJSON defaultOptions ''TestTitle)
$(deriveJSON defaultOptions ''ApStateDisplay)
$(deriveJSON defaultOptions ''DStateJSON)
$(deriveJSON defaultOptions ''ItemId)
$(deriveJSON defaultOptions ''DocActionInfo)
$(deriveJSON defaultOptions ''WhenClause)
$(deriveJSON defaultOptions ''ThenClause)
$(deriveJSON defaultOptions ''BoundaryEvent)