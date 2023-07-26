module DSL.Hook where

import Effectful as EF (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
 )
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import Path.IO (AbsPath, AnyPath, RelPath)

-- concurrency
data Once
data Thread

-- order
data Before
data After

class Before' a
class Once' a
class Thread' a
class After' a

data OnceBefore

instance Before' OnceBefore
instance Once' OnceBefore

data ThreadBefore
instance Before' ThreadBefore
instance Thread' ThreadBefore

newtype HookResult hookProps a = HookResult a

-- data Hook userEffs :: Effect where
--   OnceBefore :: Eff userEffs a -> Hook userEffs m (HookResult OnceBefore a)
--   OnceBefore' :: Eff userEffs (HookResult OnceBefore a) -> (a -> Eff userEffs b) -> Hook userEffs m (HookResult OnceBefore b)

data Hook userEffs :: Effect where
  OnceBefore :: Eff userEffs a -> Hook userEffs m (HookResult OnceBefore a)
  OnceBefore' :: m (HookResult OnceBefore a) -> (a -> Eff userEffs b) -> Hook userEffs m (HookResult OnceBefore b)
  ThreadBefore :: Eff userEffs a -> Hook userEffs m (HookResult ThreadBefore a)
  ThreadBefore' :: (Before' hc) => m (HookResult hc a) -> (a -> Eff userEffs b) -> Hook userEffs m (HookResult ThreadBefore b)
  OnceAfter :: Eff userEffs a -> Hook userEffs m (HookResult OnceBefore a)


  -- data PyrethrumTest rc tc effs where
  -- Test ::
  --   { config :: tc
  --   , items :: rc -> [i]
  --   , interactor :: rc -> i -> Eff effs as
  --   , parse :: as -> Eff '[E.Error ParseException] ds
  --   } ->
  --   PyrethrumTest rc tc effs
  -- TestNoParse ::
  --   { config :: tc
  --   , items :: rc -> [i]
  --   , interactor :: rc -> i -> Eff effs ds
  --   } ->
  --   PyrethrumTest rc tc effs

-- todo: genrate splice and use makeEffect without type
-- signatures add docs investigate renaming params
makeEffect ''Hook