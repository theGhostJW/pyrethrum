module CheckTest where

import           Check
import           Foundation.Extended
import qualified Prelude
import           Test.Extended

{-
unit_bool_ternary_true = chk $ (1 < 2) ? True $ False

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ list (linear 0 100) alpha
    reverse (reverse xs) === xs
-}

valState = 42
