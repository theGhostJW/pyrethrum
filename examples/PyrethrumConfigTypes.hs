module PyrethrumConfigTypes where

import Core qualified as C
import Data.Aeson.TH (defaultOptions, deriveJSON)

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum, Bounded)
$(deriveJSON defaultOptions ''Environment)

data Country = AU | NZ deriving (Show, Eq, Ord, Enum)
$(deriveJSON defaultOptions ''Country)

data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)
$(deriveJSON defaultOptions ''Depth)

data RunConfig = RunConfig
  { title :: Text
  , environment :: Environment
  , maxThreads :: Int
  , country :: Country
  , depth :: Depth
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''RunConfig)

instance C.Config RunConfig

defaultRunConfig :: RunConfig
defaultRunConfig = RunConfig "test" TST 1 AU DeepRegression

data FixtureConfig = MkFixtureConfig
  { title :: Text
  , depth :: Depth
  }
  deriving (Show, Eq)

newtype DefaultCfg = DefaultCfg
  { depth :: Depth
  }
  deriving (Show, Eq)

defaults :: DefaultCfg
defaults =
  DefaultCfg
    { depth = DeepRegression
    }

testConfig :: Text -> FixtureConfig
testConfig title =
  mkFull defaults
 where
  mkFull DefaultCfg{..} =
    MkFixtureConfig
      { ..
      }

$(deriveJSON defaultOptions ''FixtureConfig)

instance C.Config FixtureConfig
