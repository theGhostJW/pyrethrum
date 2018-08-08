
module Spec where

import           Foundation
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Tasty.Hedgehog ()
import           Test.Tasty.HUnit

unit_try_me = 1 @=? 1


hprop_try_me_hedgehog :: Property
hprop_try_me_hedgehog =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
