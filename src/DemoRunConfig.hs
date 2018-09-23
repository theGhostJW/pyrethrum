module DemoRunConfig where

import           Data.Set             as S
import           DemoConfigPrimatives
import           Foundation.Extended
import           TestAndRunConfig

data RunConfig = RunConfig {
  title       :: String,
  environment :: Environment,
  depth       :: Depth
} deriving Show

instance Titled RunConfig where
  title = DemoRunConfig.title

defaultRunConfig = RunConfig {
  title = "Sample RunConfig",
  environment = Test,
  depth = DeepRegression
}
