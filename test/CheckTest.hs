module CheckTest where

import           Check               as Chk
import           Foundation.Extended
import qualified Prelude
import qualified Test.Extended       as UT


-- unit_bool_ternary_true = chk $ (1 < 2) ? True $ False

-- uprop_reverse :: Property
-- uprop_reverse =
--   property $ do
--     xs <- forAll $ list (linear 0 100) alpha
--     reverse (reverse xs) === xs

chkBig = chk "Sh" (> 10)
chkEven = chk "Sh" (\a -> a `mod` 2 == 0)


tryMe = calcChecks 42 [chkBig, chkEven]
