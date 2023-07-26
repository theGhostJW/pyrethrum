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

onceBeforeHook :: forall rc tc cfs. (HasCallStack, Suite rc tc AutoEffs :> cfs) => Eff cfs (HookResult OnceBefore Int)
onceBeforeHook = onceBefore $ \rc -> pure 1

onceBeforeChildHook :: forall rc tc cfs. (HasCallStack, Suite rc tc AutoEffs :> cfs) => Eff cfs (HookResult OnceBefore Int)
onceBeforeChildHook =
  onceBefore' onceBeforeHook $
    \rc i -> do
      log $ "beforeAll' " <> txt i
      pure $ i + 1

threadBeforeHook :: forall rc tc cfs. (HasCallStack, Suite rc tc AutoEffs :> cfs) => Eff cfs (HookResult ThreadBefore Int)
threadBeforeHook = threadBefore . const $ pure 1

threadBeforeChild :: forall rc tc cfs. (HasCallStack, Suite rc tc AutoEffs :> cfs) => Eff cfs (HookResult ThreadBefore Text)
threadBeforeChild =
  threadBefore' threadBeforeHook $
    \rc i -> do
      log $ "beforeEach' " <> txt i
      pure . txt $ i + 1

threadBeforeChild2 :: forall rc tc cfs. (HasCallStack, Suite rc tc AutoEffs :> cfs) => Eff cfs (HookResult ThreadBefore Int)
threadBeforeChild2 =
  threadBefore' onceBeforeChildHook $
    \rc i -> do
      log $ "beforeEach' " <> txt i
      pure $ i + 1

-- onceBeforeHookChildHookWontCompile :: forall rc tc cfs. (HasCallStack, Suite UserEffs :> cfs) => Eff cfs (HookResult OnceBefore Int)
-- onceBeforeHookChildHookWontCompile  =
--   onceBefore' threadBeforeChild $
--     \rc i -> do
--       log $ "beforeAll' " <> txt i
--       pure $ i + 1
