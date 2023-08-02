module PyrethrumDemoTest where

import Core
import PyrethrumDemoPrj

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

beforeOnceHook :: Eff ControlEffs (Hook OnceBefore Int)
beforeOnceHook = beforeOnce $ \rc -> pure 1

beforeOnceChildHook :: Eff ControlEffs (Hook OnceBefore Int)
beforeOnceChildHook =
  beforeOnceChild beforeOnceHook $
    \rc i -> do
      log $ "beforeAll' " <> txt i
      pure $ i + 1

beforeOnceHook' :: AbstractFixtureS rc tc effs (Hook OnceBefore Integer)
beforeOnceHook' =
  BeforeOnceS
    { action = \rc -> pure 1
    }

beforeOnceChildHook' :: (Out ApEvent :> es) => AbstractFixtureS rc tc es (Hook OnceBefore (rc -> Integer -> Eff es Integer))
beforeOnceChildHook' =
  BeforeOnceChildS
    { parent = beforeOnceHook'
    , childAction =
        \rc i -> do
          log $ "beforeAll' " <> txt i
          pure $ i + 1
    }

-- beforeOnceChildHook' =
--   BeforeOnceChild beforeOnceHook' $
--     \rc i -> do
--       log $ "beforeAll' " <> txt i
--       pure $ i + 1
{-
threadBeforeHook :: Eff ControlEffs  (Hook ThreadBefore Int)
threadBeforeHook = beforeThread . const $ pure 1

threadBeforeChildInt :: Eff ControlEffs (Hook ThreadBefore Text)
threadBeforeChildInt =
  beforeThreadChild threadBeforeHook $
    \rc i -> do
      log $ "beforeEach' " <> txt i
      pure . txt $ i + 1

threadBeforeChild2 :: Eff ControlEffs (Hook ThreadBefore Int)
threadBeforeChild2 =
  beforeThreadChild beforeOnceChildHook $
    \rc i -> do
      log $ "beforeEach' " <> txt i
      pure $ i + 1
-}
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