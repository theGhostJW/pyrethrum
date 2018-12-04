
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}

module DemoProject.DemoRoughTestSimple2 where


import DemoProject.DemoRoughTestSimple as T
import DemoProject.DemoConfig
import           Foundation.Extended  hiding (Item)
import           Runner as R

data Dummy = Dummy

test :: forall effs. Effects effs => Test Item effs ApState ValState
test = T.test { configuration = config {address = moduleOf ''Dummy} }
