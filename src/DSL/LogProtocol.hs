
{-# LANGUAGE PolyKinds #-}

module DSL.LogProtocol where

import           Common (DetailedInfo, AppError)
import           Check
import           Pyrelude
import           RunElementClasses
import GHC.Generics
import OrphanedInstances
import Data.Aeson
import Data.Either
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T

newtype RunTitle = RunTitle {unRunTitle :: Text} deriving (Eq, Show, IsString)
newtype GroupTitle = GroupTitle {unGroupTitle :: Text} deriving (Eq, Show, IsString)
newtype TestTitle = TestTitle {unTestTitle :: Text} deriving (Eq, Show, IsString)
newtype ApStateDisplay = ApStateDisplay {unApStateDisplay :: Text} deriving (Eq, Show, IsString)
newtype DStateDisplay = DStateDisplay {unDStateDisplay :: Text} deriving (Eq, Show, IsString)
newtype DTestConfig = DTestConfig {unDTestConfig :: Text} deriving (Eq, Show, IsString)
newtype DRunConfig = DRunConfig {unDRunConfig :: Text} deriving (Eq, Show, IsString)
newtype WhenClause = WhenClause {unWhenClause :: Text} deriving (Eq, Show, IsString)
newtype ThenClause = ThenClause {unThenClause :: Text} deriving (Eq, Show, IsString)
data ItemId = ItemId TestModule Int deriving (Eq, Show)
data DocActionInfo = 
    ActionInfo Text |
    ActionInfoM Text Text 
    deriving (Eq, Show)

logDoc :: DocProtocol -> LogProtocol
logDoc = SubLog . Doc

logRun :: RunProtocol -> LogProtocol
logRun = SubLog . Run

data DocProtocol =   
                DocIOAction Text |
                DocAction DocActionInfo |
                DocCheck ItemId Text ResultExpectation GateStatus 
              deriving (Eq, Show)

data RunProtocol =   
                IOAction Text |
                StartPrepState |
              
                InteractorSuccess ItemId ApStateDisplay |
                InteractorFailure ItemId AppError |
              
                PrepStateSuccess ItemId DStateDisplay |
                PrepStateFailure ItemId AppError |
                CheckOutcome ItemId CheckReport 
              deriving (Eq, Show)

data SubProtocol = 
    Doc DocProtocol |
    Run RunProtocol
  deriving (Eq, Show)

data LogProtocol =
  FilterLog [FilterResult] |

  StartRun RunTitle Value | 
  EndRun |

  StartGroup GroupTitle |
  EndGroup GroupTitle |

  StartTest TestDisplayInfo |
  EndTest TestModule |

  StartIteration ItemId WhenClause ThenClause Value | 
  EndIteration ItemId |

  StartInteraction |
  StartChecks | 
  
  Message Text |
  Message' DetailedInfo |

  Warning Text |
  Warning' DetailedInfo |

  Error AppError |

  SubLog SubProtocol

  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LogProtocol)
$(deriveJSON defaultOptions ''DocProtocol)
$(deriveJSON defaultOptions ''RunProtocol)
$(deriveJSON defaultOptions ''SubProtocol)
$(deriveJSON defaultOptions ''RunTitle)
$(deriveJSON defaultOptions ''GroupTitle)
$(deriveJSON defaultOptions ''TestTitle)
$(deriveJSON defaultOptions ''ApStateDisplay)
$(deriveJSON defaultOptions ''DStateDisplay)
$(deriveJSON defaultOptions ''ItemId)
$(deriveJSON defaultOptions ''DocActionInfo)
$(deriveJSON defaultOptions ''WhenClause)
$(deriveJSON defaultOptions ''ThenClause)