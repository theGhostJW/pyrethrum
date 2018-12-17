
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- should not need this: https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}

module DemoProject.Test.Rough2 where

import DemoProject.Test.Rough as T
import DemoProject.Config
import DSL.Interpreter
import           Foundation.Extended  hiding (Item)
import           Runner as R

endpoint :: (forall a m m1. TestPlan TestConfig RunConfig FullIOEffects m1 m a) -> IO ()
endpoint = ep runConfig (IID 120)

data Dummy = Dummy

test :: forall effs. T.Effects effs => Test Item effs ApState ValState
test = T.test { configuration = config {address = moduleOf ''Dummy} }
