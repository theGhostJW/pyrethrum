module PyrethrumConfigTypes where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import CoreTypeFamilies (Config)

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

instance Config RunConfig

defaultRunConfig :: RunConfig
defaultRunConfig = RunConfig "test" TST 1 AU DeepRegression

data FixtureConfig = FxCfg
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

fxCfg :: Text -> FixtureConfig
fxCfg title =
  mkFull defaults
 where
  mkFull DefaultCfg{..} =
    FxCfg
      { ..
      }

$(deriveJSON defaultOptions ''FixtureConfig)

instance Config FixtureConfig
