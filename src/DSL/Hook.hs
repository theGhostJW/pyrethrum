module DSL.Hook where

import Core (ThreadHook (ThreadAfter))
import Effectful as EF (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
 )
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import Path.IO (AbsPath, AnyPath, RelPath)

type instance DispatchOf (Hook userEffs) = Dynamic

-- concurrency
data Once
data Thread

-- order
data Before
data After

class Before' a
class Once' a

data OnceBefore

instance Before' OnceBefore
instance Once' OnceBefore

newtype HookResult hookProps a = HookResult a

data Hook userEffs :: Effect where
  OnceBefore :: Eff userEffs a -> Hook userEffs m (HookResult OnceBefore a)
  OnceBefore' :: Eff userEffs (HookResult OnceBefore a) -> (a -> Eff userEffs b) -> Hook userEffs m (HookResult OnceBefore b)

-- todo: genrate splice and use makeEffect without type
-- signatures add docs investigate renaming params
makeEffect ''Hook