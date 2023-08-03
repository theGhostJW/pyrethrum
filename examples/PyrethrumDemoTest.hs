module PyrethrumDemoTest where

import Core (OnceBefore)
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

int_hook :: Fixture (OnceBefore Int)
int_hook =
  OnceBefore
    { action = \rc -> pure 1
    }

add_int_hook :: Fixture (OnceBefore (RunConfig -> Int -> Suite Int))
add_int_hook =
  ChildOnceBefore
    { parent = int_hook
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
-- test :: Test
-- test = Full config action parse items

-- config :: TestConfig
-- config = TestConfig "test" DeepRegression

-- action :: (Out ApEvent :> es, Show b, Num b) => p -> b -> Eff es b
-- action rc i = do
--   log $ txt i
--   pure $ i + 1
-- parse = pure
-- items = const [1, 2, 3]