module TestFilterTest where

import Pyrelude         as P
import Polysemy
import           Runner as R
import           Pyrelude.Test hiding (Group)
import Data.Yaml
import Data.Aeson.TH
import Data.Aeson.Types
import TestFilter
import MockSuite
import RunnerBase as RB ( Test, AddressedElm (..) )
import Check (Checks)



filterResults :: [TestFilter RunConfig TestConfig] -> RunConfig ->  [AddressedElm TestFilterResult]
filterResults = filterLog mockSuite 

data Status = Accepted | Rejected

tests :: RunConfig -> [TestFilter RunConfig TestConfig] ->  Status -> [ShowFilter]
tests rc fltrs s = 
  let 
    matchStatus sf = 
      let 
        m = rejection sf 
      in
       s == Accepted ? isNothing  m $ isJust m
  in
   P.filter matchStatus $ showIt . RB.element <$> filterResults fltrs rc


data ShowFilter = ShowFilter {
  name :: Text,
  rejection :: Maybe Text
} deriving Show

showIt :: TestFilterResult -> ShowFilter
showIt r = ShowFilter ( (title :: TestLogInfo -> Text) $ testInfo r) (reasonForRejection  r)

includeCfg :: RunConfig
includeCfg =
  RunConfig
    { title = "Include Flag",
      includeFlag = Just True,
      testTitle = Nothing
    }


-- $> infilter
infilter :: [TestFilterResult]
infilter = acceptedTests includeCfg

-- chkFilters :: [Text] -> RunConfig -> Assertion
-- chkFilters expted rc = chkEq expted $ title . testInfo <$> acceptedTests rc

-- unit_test_filter_expect_empty :: Assertion
-- unit_test_filter_expect_empty = chkFilters [] $ RunConfig NZ Connectivity

-- unit_test_filter_country :: Assertion
-- unit_test_filter_country = chkFilters ["test1", "test3"] $ RunConfig Au Regression

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