module PyrethrumDemo where

import Core
import Data.Aeson.TH
import Data.Aeson.Types (
  FromJSON,
  ToJSON (toJSON),
  Value (String),
  parse,
 )

import DSL.FileSystemEffect
import qualified DSL.FileSystemEffect as IOI
import qualified DSL.Internal.ApEvent as AE
import DSL.Out
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Dynamic (Error)

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

type AppEffs = '[FileSystem, Out AE.ApEvent, Error Text, IOE]

type Test = PyrethrumTest RunConfig TestConfig AppEffs

log :: (Out AE.ApEvent :> es) => Text -> Eff es ()
log = out . AE.Log

parent :: Hook AppEffs Integer
parent = Hook {
  title = "parent",
  action = do
    log "parent run"
    pure 3
}

-- child :: Hook AppEffs Text
-- child = 
--   withHook parent $ \i ->
--   pure $ Hook {
--   title = "child",
--   action = do
--     let msg = repeat "Hi"
--     log "Hi"
--     pure "child"
-- }

-- Hook interpretor that runs hook with mute out and oc effects
