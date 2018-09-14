module CheckTest where

import           Check               as Chk
import           Foundation.Extended
import qualified Prelude             as P
import qualified Test.Extended       as UT


-- unit_bool_ternary_true = chk $ (1 < 2) ? True $ False

-- uprop_reverse :: Property
-- uprop_reverse =
--   property $ do
--     xs <- forAll $ list (linear 0 100) alpha
--     reverse (reverse xs) === xs

isBig = chk "More than 10" (> 10)
isEven = chk "Even" P.even
isOdd' = chk' "Odd" P.odd
isOdd = chk "Odd" P.odd

isOddm' = chkm' "Odd Test" P.odd $ \v -> "The value was: " <> show v <> " and is expectedf to be odd"
isOddm = chkm "Odd Test" P.odd $ \v -> "The value was: " <> show v <> " and is expectedf to be odd"

tryMe = outcome <$> calcChecks 42 (isOdd' <> isBig <> isEven)

chkOutcomes expected val checks = UT.chkEq (fromList expected) $ outcome <$> calcChecks val checks

unit_chkOutcomes_full_success = chkOutcomes [Pass, Pass] 42 $ isBig <> isEven
unit_chkOutcomes_fail_and_success = chkOutcomes [Fail, Pass, Pass] 42 $ isOdd <> isBig <> isEven
unit_chkOutcomes_fail_and_success2 = chkOutcomes [Pass, Pass, Fail] 42 $ isBig <> isEven <> isOdd

unit_chkOutcomes_exception_and_skip_and_success2 = chkOutcomes [Pass, Exception, Skip] 42 (isBig <> isOdd' <> isEven)

unit_chkOutcomes_inlined = chkOutcomes [Pass, Exception, Skip] 42 $ chk "More than 10" (> 10)  <>
                                                                    chk' "Odd" P.odd <>
                                                                    chk "Even" P.even

unit_chkWithChkm' = chkOutcomes [Pass, Pass, Exception, Skip, Skip]   42  $ isBig
                                                                         <> isEven
                                                                         <> isOddm'
                                                                         <> isOdd
                                                                         <> isEven

unit_chkWithChkm = chkOutcomes [Pass, Pass, Fail, Fail, Pass] 42  $ isBig
                                                                  <> isEven
                                                                  <> isOddm
                                                                  <> isOdd
                                                                  <> isEven

_inspect = calcChecks 42  $ isBig
                          <> isEven
                          <> isOddm
                          <> isOdd
                          <> isEven
