
{-# LANGUAGE PolyKinds #-}

module DSL.LogProtocol where

import           DSL.Common (DetailedInfo, AppError)
import           TestFilter
import           Foundation.Extended
import           TestAndRunConfig
import GHC.Generics
import OrphanedInstances
import Data.Aeson
import Data.Either
import Data.Aeson.Types
import Data.Aeson.TH
import RunnerBase as RB
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T

data LogProtocol =
  Message String |
  Message' DetailedInfo |

  Warning String |
  Warning' DetailedInfo |

  IOAction String |

  Error AppError |
  FilterLog [FilterResult] |

  StartRun String Value |  -- title / runconfig
  StartGroup String |
  StartTest TestDisplayInfo |
  StartIteration TestModule Int Value | -- iid / test module / item
  EndIteration TestModule Int String  | -- test module / iid / test Info
  EndRun

  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LogProtocol)