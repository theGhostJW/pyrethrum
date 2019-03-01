
{-# LANGUAGE PolyKinds #-}

module DSL.LogProtocol where

import           Common (DetailedInfo, AppError)
import           TestFilter
import           Foundation.Extended
import           RunElementClasses
import GHC.Generics
import OrphanedInstances
import Data.Aeson
import Data.Either
import Data.Aeson.Types
import Data.Aeson.TH
import RunnerBase as RB
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T

newtype RunTitle = RunTitle {unRunTitle :: String} deriving (Eq, Show, IsString)
newtype GroupTitle = GroupTitle {unGroupTitle :: String} deriving (Eq, Show, IsString)
newtype TestTitle = TestTitle {unTestTitle :: String} deriving (Eq, Show, IsString)

$(deriveJSON defaultOptions ''RunTitle)
$(deriveJSON defaultOptions ''GroupTitle)
$(deriveJSON defaultOptions ''TestTitle)

data LogProtocol =
  Message String |
  Message' DetailedInfo |

  Warning String |
  Warning' DetailedInfo |

  IOAction String |

  Error AppError |
  FilterLog [FilterResult] |

  StartRun RunTitle Value |  -- title / runconfig
  EndRun |

  StartGroup GroupTitle |
  EndGroup GroupTitle |

  StartTest TestDisplayInfo |
  EndTest TestModule |

  StartIteration TestModule Int Value | -- test / iid / module / item
  Result TestModule Int String | -- test module / iid / test Info
  EndIteration TestModule Int -- test module / iid


  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LogProtocol)