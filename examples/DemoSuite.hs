module DemoSuite where

import Core as C
import DSL.FileSystemEffect
import DSL.Internal.ApEvent
import DSL.Out
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, runError)
import PyrethrumExtras (txt)

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

-- direct 

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

-- object 


-- beforeOnceHookChildHookWontCompile :: forall rc tc cfs. (HasCallStack, Suite UserEffs :> cfs) => Eff cfs (HookResult beforeOnce Int)
-- beforeOnceHookChildHookWontCompile  =
--   beforeOnce' $
--     \rc i -> do
--       log $ "beforeAll' " <> txt i
--       pure $ i + 1
