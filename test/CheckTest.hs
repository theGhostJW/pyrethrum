module CheckTest where

import           Check               as Chk
import           Data.Function
import           Foundation.Extended
import qualified Prelude             as P
import qualified Test.Extended       as UT

isBig = chk "More than 10" (> 10)
isEven = chk "Even" P.even
isOdd = chk "Odd" P.odd

isOddGate = gate $ chk' "Odd Test"
                   (\v -> "The value was: " <> show v <> " and is expected to be odd")
                   P.odd

_demo = outcome <$> calcChecks 42 (isOddGate <> isBig <> isEven)

chkOutcomes expected val checks = UT.chkEq (fromList expected) $ outcome <$> calcChecks val checks

unit_chk_outcomes_full_success = chkOutcomes [Pass, Pass] 42 $ isBig <> isEven
unit_chk_outcomes_fail_and_success = chkOutcomes [Fail, Pass, Pass] 42 $ isOdd <> isBig <> isEven
unit_chk_outcomes_fail_and_success2 = chkOutcomes [Pass, Pass, Fail] 42 $ isBig <> isEven <> isOdd

unit_chk_outcomes_gateFail_and_skip_and_success2 = chkOutcomes [Pass, GateFail, Skip] 42 (isBig <> isOddGate <> isEven)

unit_chk_outcomes_inlined_gate = chkOutcomes [Pass, GateFail, Skip] 42 $ chk "More than 10" (> 10)  <>
                                                                    gate (chk "Odd" P.odd) <>
                                                                    chk "Even" P.even

unit_chk_with_with_gate_fail = chkOutcomes [Pass, Pass, GateFail, Skip]   42 $ isBig
                                                                           <> isEven
                                                                           <> gate isOdd
                                                                           <> isEven

unit_chk_with_chkGate_success = chkOutcomes [Pass, Pass, GateFail, Skip, Skip] 42  $ isBig
                                                                  <> isEven
                                                                  <> isOddGate
                                                                  <> isOdd
                                                                  <> isEven
evenOddEven = isEven
              <> isOdd
              <> isOdd
              <> isEven

unit_chk_with_gate_on_list = chkOutcomes [Pass, Pass, GateFail, Skip, Skip, Skip] 42 $ isBig
                                                                  <> gate evenOddEven
                                                                  <> isEven


_checkmDemo = calcChecks 42  $ isBig
                             <> isEven
                             <> isOddGate
                             <> isOdd
                             <> isEven
