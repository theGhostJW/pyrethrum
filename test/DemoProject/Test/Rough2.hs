
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- should not need this: https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}

module DemoProject.Test.Rough2 where

import DemoProject.Test.Rough as T
import DemoProject.Config
import DSL.Interpreter
import           Polysemy
import           Pyrelude
import           Runner as R

endpoint :: (forall m1 m a. TestPlan m1 m a FullIOEffects) -> Sem FullIOEffects ()
endpoint = ep runConfig (IID 120)

data Dummy = Dummy

test :: forall effs. T.Effects effs => Test Item effs ApState DState
test = T.test { configuration = config {address = mkTestModule ''Dummy} }