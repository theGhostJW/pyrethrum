
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- https://github.com/haskell/haskell-ide-engine/issues/842
{-# LANGUAGE QuasiQuotes #-}

module DemoProject.RoughTestSimple2 where


import DemoProject.RoughTestSimple as T
import DemoProject.Config
import           Foundation.Extended  hiding (Item)
import           Runner as R

data Dummy = Dummy

test :: forall effs. Effects effs => Test Item effs ApState ValState
test = T.test { configuration = config {address = moduleOf ''Dummy} }
