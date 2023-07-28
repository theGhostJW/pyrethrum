module PyrethrumDemo where

import Core as C
import Data.Aeson.TH
import Data.Aeson.Types (
  FromJSON,
  ToJSON (toJSON),
  Value (String),
  parse,
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

type AutoEffs = '[FileSystem, Out ApEvent, Error FSException, IOE]
type ApEffs es = Eff '[FileSystem, Out ApEvent, Error FSException, IOE] es
type ApConstraints es = (FileSystem :> es, Out ApEvent :> es, Error FSException :> es, IOE :> es)
type ControlEffs es = Eff '[FileSystem, Out ApEvent, Error FSException, IOE] es

log :: (Out ApEvent :> es) => Text -> Eff es ()
log = out . Log


-- object 


-- onceBeforeHookChildHookWontCompile :: forall rc tc cfs. (HasCallStack, Suite UserEffs :> cfs) => Eff cfs (HookResult OnceBefore Int)
-- onceBeforeHookChildHookWontCompile  =
--   onceBefore' threadBeforeChild $
--     \rc i -> do
--       log $ "beforeAll' " <> txt i
--       pure $ i + 1


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

type Test = TestParams RunConfig TestConfig AppEffs


-- set up elements

beforeOnceHook :: forall rc tc cfs. (HasCallStack, Suite rc tc AutoEffs :> cfs) => Eff cfs (HookResult OnceBefore Int)
beforeOnceHook = beforeOnce $ \rc -> pure 1

beforeOnceChildHook :: forall rc tc cfs. (HasCallStack, Suite rc tc AutoEffs :> cfs) => Eff cfs (HookResult OnceBefore Int)
beforeOnceChildHook =
  beforeOnceChild beforeOnceHook $
    \rc i -> do
      log $ "beforeAll' " <> txt i
      pure $ i + 1

threadBeforeHook :: forall rc tc cfs. (HasCallStack, Suite rc tc AutoEffs :> cfs) => Eff cfs (HookResult ThreadBefore Int)
threadBeforeHook = beforeThread . const $ pure 1

threadBeforeChildInt :: forall rc tc cfs. (HasCallStack, Suite rc tc AutoEffs :> cfs) => Eff cfs (HookResult ThreadBefore Text)
threadBeforeChildInt =
 beforeThreadChild threadBeforeHook $
    \rc i -> do
      log $ "beforeEach' " <> txt i
      pure . txt $ i + 1

threadBeforeChild2 :: forall rc tc cfs. (HasCallStack, Suite rc tc AutoEffs :> cfs) => Eff cfs (HookResult ThreadBefore Int)
threadBeforeChild2 =
 beforeThreadChild beforeOnceChildHook $
    \rc i -> do
      log $ "beforeEach' " <> txt i
      pure $ i + 1


-- parent :: Hook AppEffs Integer
-- parent = Hook {
--   title = "parent",
--   action = do
--     log "parent run"
--     pure 3
-- }

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
