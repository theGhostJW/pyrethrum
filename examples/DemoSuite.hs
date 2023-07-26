module DemoSuite where

import qualified Core as C
import DSL.FileSystemEffect
import DSL.Hook
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

type UserEffs = '[FileSystem, Out ApEvent, Error FSException, IOE] 
type ApEffs es = Eff '[FileSystem, Out ApEvent, Error FSException, IOE] es
type ApConstraints es = (FileSystem :> es, Out ApEvent :> es, Error FSException :> es, IOE :> es)
type ControlEffs es = Eff '[FileSystem, Out ApEvent, Error FSException, IOE] es

log :: (Out ApEvent :> es) => Text -> Eff es ()
log = out . Log

onceBeforeParentHook :: forall ceffs ueffs. (HasCallStack, Hook ueffs:> ceffs) => Eff ceffs (HookResult OnceBefore Int)
onceBeforeParentHook = onceBefore $ pure 1

onceBeforeChildHook :: forall cfs. (HasCallStack, Hook UserEffs :> cfs) => Eff cfs (HookResult OnceBefore Int)
onceBeforeChildHook =
  onceBefore' onceBeforeParentHook $
    \i -> do
      log $ "beforeAll' " <> txt i
      pure $ i + 1

-- beforeEach :: forall cfs. (HasCallStack, Hook :> cfs) => Eff cfs (HookResult ThreadBefore Int)
-- beforeEach = threadBefore $ pure 1

-- beforeEach' :: forall cfs. (HasCallStack, Hook :> cfs, Out ApEvent :> cfs) => Eff cfs (HookResult ThreadBefore Int)
-- beforeEach' =
--   threadBefore' beforeEach $
--     \i -> do
--       log $ "beforeEach' " <> txt i
--       pure $ i + 1

-- beforeEach'' :: forall cfs. (HasCallStack, Hook :> cfs, Out ApEvent :> cfs) => Eff cfs (HookResult ThreadBefore Int)
-- beforeEach'' =
--   threadBefore' beforeAll $
--     \i -> do
--       log $ "beforeEach' " <> txt i
--       pure $ i + 1



-- beforeAllWontCompile :: forall cfs. (HasCallStack, Hook :> cfs, Out ApEvent :> cfs) => Eff cfs (HookResult OnceBefore Int)
-- beforeAllWontCompile =
--   onceBefore' beforeEach $
--     \i -> do
--       log $ "beforeAll' " <> txt i
--       pure $ i + 1
