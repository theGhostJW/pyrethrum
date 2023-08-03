module PyrethrumDemoPrj where

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


type ApEffs = '[FileSystem, Out ApEvent, Error FSException, IOE]
type Suite es = Eff '[FileSystem, Out ApEvent, Error FSException, IOE] es
type ApConstraints es = (FileSystem :> es, Out ApEvent :> es, Error FSException :> es, IOE :> es)
type AppEffs a = forall es. (FileSystem :> es, Out ApEvent :> es, Error FSException :> es, IOE :> es) => Eff es a



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

instance C.Config RunConfig

data TestConfig = TestConfig
  { title :: Text
  , depth :: Depth
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestConfig)

instance C.Config TestConfig

type Test = C.AbstractTest RunConfig TestConfig ApEffs

-- type Fixture a = AbstractFixture RunConfig TestConfig ApEffs a

data Fixture a where
  OnceBefore ::
    { action :: RunConfig -> Suite a
    } ->
    Fixture (C.OnceBefore a)
  ChildOnceBefore ::
    { parent :: Fixture (C.OnceBefore a)
    , childAction :: RunConfig -> a -> Suite b
    } ->
    Fixture (C.OnceBefore (RunConfig-> a -> Suite b))

makeAbstract :: Fixture a -> C.AbstractFixture RunConfig TestConfig ApEffs a
makeAbstract = \case 
  OnceBefore action -> C.OnceBefore action
  ChildOnceBefore parent childAction -> C.ChildOnceBefore (makeAbstract parent) childAction