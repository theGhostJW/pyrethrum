module PyrethrumDemoTest where

import PyrethrumDemoPrj
import Core
-- import qualified Core as C
-- import Data.Aeson.TH
-- import Data.Aeson.Types (
--   FromJSON,
--   ToJSON (toJSON),
--   Value (String)
--  )

-- import qualified DSL.FileSystemEffect as IOI
-- import qualified DSL.Internal.ApEvent as AE

-- import DSL.FileSystemEffect
import DSL.Internal.ApEvent
import DSL.Out
import Effectful (Eff, IOE, (:>))
-- import Effectful.Error.Static (Error, runError)
import PyrethrumExtras (txt)


log :: (Out ApEvent :> es) => Text -> Eff es ()
log = out . Log

beforeOnceHook :: Fixture (Hook OnceBefore Int)
beforeOnceHook = beforeOnce $ \rc -> pure 1

beforeOnceChildHook :: Fixture (Hook OnceBefore Int)
beforeOnceChildHook =
  beforeOnceChild beforeOnceHook $
    \rc i -> do
      log $ "beforeAll' " <> txt i
      pure $ i + 1

threadBeforeHook :: Fixture (Hook ThreadBefore Int)
threadBeforeHook = beforeThread . const $ pure 1

threadBeforeChildInt :: Fixture (Hook ThreadBefore Text)
threadBeforeChildInt =
  beforeThreadChild threadBeforeHook $
    \rc i -> do
      log $ "beforeEach' " <> txt i
      pure . txt $ i + 1

threadBeforeChild2 :: Fixture (Hook ThreadBefore Int)
threadBeforeChild2 =
  beforeThreadChild beforeOnceChildHook $
    \rc i -> do
      log $ "beforeEach' " <> txt i
      pure $ i + 1

test :: Test
test = Full config action parse items

config :: TestConfig
config = TestConfig "test" DeepRegression

action :: (Out ApEvent :> es, Show b, Num b) => p -> b -> Eff es b
action rc i = do
  log $ txt i
  pure $ i + 1
parse = pure
items = const [1, 2, 3]


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
