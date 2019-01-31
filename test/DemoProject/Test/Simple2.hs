
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}

module DemoProject.Test.Simple2 where
  
import DemoProject.Test.Simple as T
import DemoProject.Config
import DSL.Interpreter
import           Foundation.Extended  hiding (Item)
import           Runner as R
import           TestAndRunConfig

endpoint :: (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
endpoint = ep runConfig (IID 120)

data Dummy = Dummy

test :: forall effs. Effects effs => Test Item effs ApState ValState
test = T.test { configuration = config {address = mkTestModule ''Dummy} }
