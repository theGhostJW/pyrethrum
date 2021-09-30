module TestFilterTest where

import Check (Checks)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import MockSuite hiding (filters')
import Polysemy
import Pyrelude as P
import Pyrelude.Test hiding (Group)
import Runner as R
import RunnerBase as RB (AddressedElm (..), Test, querySuite)
import TestFilter
import Text.Show.Pretty

-- $ > view allTossCalls
allTossCalls :: [(Text, TossCall)]
allTossCalls =
  let titleAndCall :: a -> hi -> (hi -> Sem effs ho)  -> (ho -> Sem effs ()) -> MockTest ho i as ds effs -> (Text, TossCall)
      titleAndCall _ _ _ _ (Test (TestConfig ttl call) _ _ _) = (ttl, call)

      root = mockSuite titleAndCall
   in RB.element <$> querySuite fst root

baseCfg :: TossResult -> RunConfig
baseCfg tr =
  RunConfig
    { title = "Unit Test Config",
      toss = tr
    }

filterResults :: [TestFilter RunConfig TestConfig] -> RunConfig -> [TestFilterResult]
filterResults = filterLog mockSuite

data Status = Accepted | Rejected | AnyResult deriving (Eq)

type ShowFilter = (Text, Maybe Text)

tests' :: RunConfig -> [TestFilter RunConfig TestConfig] -> Status -> [ShowFilter]
tests' rc fltrs s =
  let matchStatus sf = s == AnyResult || (s == Accepted ? isNothing $ isJust) (snd sf)
   in P.filter matchStatus $ showIt <$> filterResults fltrs rc

showIt :: TestFilterResult -> ShowFilter
showIt r = ((title :: TestLogInfo -> Text) $ testInfo r, reasonForRejection r)

filters' :: Maybe Text -> [TestFilter RunConfig TestConfig]
filters' ttl = [tossFilter, hasTitle ttl]

view :: Show a => [a] -> IO ()
view = pPrintList

-- $ > view allTests
allTests :: [ShowFilter]
allTests = tests' (baseCfg RcAll) (filters' Nothing) Accepted

-- $ > unit_test_any_result_has_all
unit_test_any_result_has_all :: Assertion
unit_test_any_result_has_all = chkEq (length allTests) (length headsAll)


-- $ > view headsAll
headsAll :: [ShowFilter]
headsAll = tests' (baseCfg RcHeads) (filters' Nothing) AnyResult

-- $ > view headsRejected
headsRejected :: [ShowFilter]
headsRejected = tests' (baseCfg RcHeads) (filters' Nothing) Rejected

-- $ > view headsRejected
headsAccepted :: [ShowFilter]
headsAccepted = tests' (baseCfg RcHeads) (filters' Nothing) Accepted

-- $ > unit_test_all_heads
unit_test_all_heads :: Assertion
unit_test_all_heads = chkEq ( fst <$> headsAccepted) ["test1", "test4", "test6"]

-- $ > view headsWith6
headsWith6 :: [ShowFilter]
headsWith6 = tests' (baseCfg RcHeads) (filters' $ Just "6") Accepted

-- $ > view headsWith6Rejects
headsWith6Rejects :: [ShowFilter]
headsWith6Rejects = tests' (baseCfg RcHeads) (filters' $ Just "6") Rejected

-- $ > unit_check_rejection_messages
unit_check_rejection_messages :: Assertion
unit_check_rejection_messages = chkEq headsWith6Rejects [ ( "test1" , Just "test title must include: 6" )
                                                        , ( "test4" , Just "test title must include: 6" )
                                                        , ( "test5" , Just "toss call: Tails must match run: RcHeads" )
                                                        , ( "test2" , Just "toss call: Tails must match run: RcHeads" )
                                                        , ( "test3" , Just "toss call: Tails must match run: RcHeads" )
                                                        ]


-- unit_test_filter_country_nz :: Assertion
-- unit_test_filter_country_nz = chkFilters ["test1", "test2"] $ RunConfig NZ Regression

-- unit_test_filter_country_au_deep_regression :: Assertion
-- unit_test_filter_country_au_deep_regression = chkFilters ["test1", "test3", "test4"] $ RunConfig Au DeepRegression

-- filtersExcludeReasons :: RunConfig -> [Text]
-- filtersExcludeReasons rc = catMaybes $ reasonForRejection <$> P.filter rejectFilter (filterResults rc)

-- unit_test_filter_exclude_reasons :: Assertion
-- unit_test_filter_exclude_reasons = chkEq [
--                                           "depth must be within run parameters (e.g. regression test will not be run in connectiviity run)",
--                                           "depth must be within run parameters (e.g. regression test will not be run in connectiviity run)",
--                                           "country must match test run",
--                                           "country must match test run",
--                                           "test must be is enabled"
--                                           ]
--                                           $ filtersExcludeReasons $ RunConfig NZ Connectivity

--}