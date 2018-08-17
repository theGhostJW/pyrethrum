
module InteractorSpikeTest where

import           Foundation
import qualified Prelude
import           Test.Extended

unit_try_me = chkEq 1 1

{-# ANN module ("HLint: ignore Avoid reverse":: Prelude.String) #-}

hprop_try_me_hedgehog :: Property
hprop_try_me_hedgehog =
  property $ do
    xs <- forAll $ list (linear 0 100) alpha
    reverse (reverse xs) === xs
