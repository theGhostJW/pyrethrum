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
isOdd = chk "Odd" P.odd

isOddGuard = guard $ chk' "Odd Test"
                   (\v -> "The value was: " <> show v <> " and is expected to be odd")
                   P.odd

tryMe = outcome <$> calcChecks 42 (isOddGuard <> isBig <> isEven)

chkOutcomes expected val checks = UT.chkEq (fromList expected) $ outcome <$> calcChecks val checks

unit_chk_outcomes_full_success = chkOutcomes [Pass, Pass] 42 $ isBig <> isEven
unit_chk_outcomes_fail_and_success = chkOutcomes [Fail, Pass, Pass] 42 $ isOdd <> isBig <> isEven
unit_chk_outcomes_fail_and_success2 = chkOutcomes [Pass, Pass, Fail] 42 $ isBig <> isEven <> isOdd

unit_chk_outcomes_guardFail_and_skip_and_success2 = chkOutcomes [Pass, GuardFail, Skip] 42 (isBig <> isOddGuard <> isEven)

unit_chk_outcomes_inlined_guard = chkOutcomes [Pass, GuardFail, Skip] 42 $ chk "More than 10" (> 10)  <>
                                                                    guard (chk "Odd" P.odd) <>
                                                                    chk "Even" P.even

unit_chk_with_with_guard_fail = chkOutcomes [Pass, Pass, GuardFail, Skip]   42 $ isBig
                                                                           <> isEven
                                                                           <> guard isOdd
                                                                           <> isEven

unit_chk_with_chkGuard_success = chkOutcomes [Pass, Pass, GuardFail, Skip, Skip] 42  $ isBig
                                                                  <> isEven
                                                                  <> isOddGuard
                                                                  <> isOdd
                                                                  <> isEven
evenOddEven = isEven
              <> isOdd
              <> isOdd
              <> isEven

unit_chk_with_guard_on_list = chkOutcomes [Pass, Pass, GuardFail, Skip, Skip, Skip] 42  $ isBig
                                                                  <> guard evenOddEven
                                                                  <> isEven


_checkmDemo = calcChecks 42  $ isBig
                             <> isEven
                             <> isOddGuard
                             <> isOdd
                             <> isEven
