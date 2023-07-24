module DemoSuite where

import qualified Core as C
import DSL.Hook
import Effectful (Eff, (:>))

{-
 - Suite elements 
  - only expose data constructors to users not lifted constructors
  - only expose user effects to user 
  - interpretors internal 
    - may or may not require sub-interpreter
    - suite interpretor double parameterised
  - first run interpretors as required 
-}

declare contol effects here

beforeAll :: forall cfs es. (HasCallStack, Hook es :> cfs) => Eff cfs (HookResult OnceBefore Int)
beforeAll = onceBefore $ pure 1


beforeAll'' :: forall cfs es. (HasCallStack, Hook  es :> cfs) => Eff cfs (HookResult OnceBefore Int)
beforeAll'' = onceBefore' beforeAll $ \i -> pure $ i + 1