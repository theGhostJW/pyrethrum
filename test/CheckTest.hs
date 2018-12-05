module CheckTest where

import           Check               as Chk
import           Data.Function
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
isOddGuard = guard $ chk "Odd" P.odd
isOdd = chk "Odd" P.odd

isOddGuardm = guard $ chkGuard "Odd Test"
                            P.odd $
                            \v -> "The value was: " <> show v <> " and is expected to be odd"
isOddm = chkGuard "Odd Test"
               P.odd $
               \v -> "The value was: " <> show v <> " and is expectedf to be odd"

tryMe = outcome <$> calcChecks 42 (isOddGuard <> isBig <> isEven)

chkOutcomes expected val checks = UT.chkEq (fromList expected) $ outcome <$> calcChecks val checks

unit_chk_outcomes_full_success = chkOutcomes [Pass, Pass] 42 $ isBig <> isEven
unit_chk_outcomes_fail_and_success = chkOutcomes [Fail, Pass, Pass] 42 $ isOdd <> isBig <> isEven
unit_chk_outcomes_fail_and_success2 = chkOutcomes [Pass, Pass, Fail] 42 $ isBig <> isEven <> isOdd

unit_chk_outcomes_exception_and_skip_and_success2 = chkOutcomes [Pass, Exception, Skip] 42 (isBig <> isOddGuard <> isEven)

unit_chk_outcomes_inlined_guard = chkOutcomes [Pass, Exception, Skip] 42 $ chk "More than 10" (> 10)  <>
                                                                    guard (chk "Odd" P.odd) <>
                                                                    chk "Even" P.even

unit_chk_with_chkGuard_guard = chkOutcomes [Pass, Pass, Exception, Skip, Skip]   42 $ isBig
                                                                           <> isEven
                                                                           <> isOddGuardm
                                                                           <> isOdd
                                                                           <> isEven

unit_chk_with_chkGuard = chkOutcomes [Pass, Pass, Fail, Fail, Pass] 42  $ isBig
                                                                  <> isEven
                                                                  <> isOddm
                                                                  <> isOdd
                                                                  <> isEven

_checkmDemo = calcChecks 42  $ isBig
                             <> isEven
                             <> isOddm
                             <> isOdd
                             <> isEven
