module DemoConfig where

import           Pyrelude as P
import RunElementClasses
import Data.Set as S
import qualified Data.Aeson as A
import Data.ByteString.Lazy as B
import Data.Aeson.TH
import DSL.LogProtocol
import Common


data SuiteError = MyError Text | 
                  MyOtherError 
                  deriving (Show, Typeable)

type AppError = FrameworkError SuiteError

type LogProtocol = LogProtocolBase SuiteError

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum)
data Country = AU | NZ deriving (Show, Eq, Ord, Enum)
data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)

data TestConfig = TestConfig {
  header       :: Text,
  address      :: TestAddress,
  environments :: Set Environment,
  countries    :: Set Country,
  minDepth     :: Depth,
  active       :: Bool
}  deriving (Eq, Show)

data RunConfig = RunConfig {
  runTitle    :: Text,
  environment :: Environment,
  country     :: Country,
  depth       :: Depth
} deriving (Eq, Show)

runConfig :: RunConfig
runConfig = RunConfig {
  runTitle = "Sample RunConfig",
  environment = TST,
  country = AU,
  depth = DeepRegression
}


$(deriveJSON defaultOptions ''TestConfig)
$(deriveJSON defaultOptions ''Environment)
$(deriveJSON defaultOptions ''Country)
$(deriveJSON defaultOptions ''Depth)
$(deriveJSON defaultOptions ''RunConfig)
