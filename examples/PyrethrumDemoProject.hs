module PyrethrumDemoProject where

import Core
import qualified Core as C
import Data.Aeson.TH
import Data.Aeson.Types (
  FromJSON,
  ToJSON (toJSON),
  Value (String)
 )

import qualified DSL.FileSystemEffect as IOI
import qualified DSL.Internal.ApEvent as AE

import DSL.FileSystemEffect
import DSL.Internal.ApEvent
import DSL.Out
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, runError)
import PyrethrumExtras (txt)

-- import DSL.Hook

{-
 - Suite elements
  - only expose data constructors to users not lifted constructors
  - only expose user effects to user
  - interpretors internal
    - may or may not require sub-interpreter
    - suite interpretor double parameterised
  - first run interpretors as required
-}

type AppEffs = '[FileSystem, Out ApEvent, Error FSException, IOE]
type App es = Eff '[FileSystem, Out ApEvent, Error FSException, IOE] es
type ApConstraints es = (FileSystem :> es, Out ApEvent :> es, Error FSException :> es, IOE :> es)
type ControlEffs es = Eff '[FileSystem, Out ApEvent, Error FSException, IOE] es

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum, Bounded)
$(deriveJSON defaultOptions ''Environment)

data Country = AU | NZ deriving (Show, Eq, Ord, Enum)
$(deriveJSON defaultOptions ''Country)

data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)
$(deriveJSON defaultOptions ''Depth)

data RunConfig = RunConfig
  { title :: Text
  , environment :: Environment
  , country :: Country
  , depth :: Depth
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''RunConfig)

instance Config RunConfig

data TestConfig = TestConfig
  { title :: Text
  , depth :: Depth
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestConfig)

instance Config TestConfig

type Test = AbstractTest RunConfig TestConfig AppEffs

type Fixture a = Eff '[Suite RunConfig TestConfig AppEffs] a


