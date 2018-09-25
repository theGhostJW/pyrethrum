module DemoRunConfig where

import           Data.Set             as S
import           DemoConfigPrimatives
import           Foundation.Extended
import           TestAndRunConfig

data RunConfig = RunConfig {
  runTitle    :: String,
  environment :: Environment,
  depth       :: Depth
} deriving Show

instance Titled RunConfig where
  title = runTitle
