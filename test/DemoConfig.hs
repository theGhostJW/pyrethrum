module DemoConfig where

import Common
import DSL.LogProtocol
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Lazy as B
import Data.Set as S
import RunElementClasses

data SuiteError
  = MyError Text
  | MyOtherError
  deriving (Show, Typeable)

type AppError = FrameworkError SuiteError

type LogProtocol = LogProtocolBase SuiteError

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum)

$(deriveJSON defaultOptions ''Environment)

data Country = AU | NZ deriving (Show, Eq, Ord, Enum)

$(deriveJSON defaultOptions ''Country)

data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)

$(deriveJSON defaultOptions ''Depth)

data TestConfig = TestConfig
  { header :: Text,
    environments :: Set Environment,
    countries :: Set Country,
    minDepth :: Depth,
    active :: Bool
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''TestConfig)

data RunConfig = RunConfig
  { runTitle :: Text,
    environment :: Environment,
    country :: Country,
    depth :: Depth
  }
  deriving (Eq, Show)

runConfig :: RunConfig
runConfig =
  RunConfig
    { runTitle = "Sample RunConfig",
      environment = TST,
      country = AU,
      depth = DeepRegression
    }

$(deriveJSON defaultOptions ''RunConfig)
