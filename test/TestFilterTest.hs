module TestFilterTest where

import Check (Checks)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import MockSuite hiding (filters')
import Polysemy
import Pyrelude as P
import Pyrelude.Test hiding (Group)
import RunElementClasses
import Runner as R
import RunnerBase as RB (AddressedElm (..), Test, TestInfo (..), querySuite, querySuite')
import TestFilter
import Text.Show.Pretty
import GHC.Records
import DSL.Interpreter
import DSL.Logger
import MemberReflection (showEffs)

import EvalHelp


-- $ > view allTossCalls
allTossCalls :: [(Text, TossCall)]
allTossCalls =
  let titleAndCall :: rc -> Address -> MockTest ho i as ds effs -> (Text, TossCall)
      titleAndCall _ _ (R.Test (TestConfig ttl call) _ _ _) = (ttl, call)

      title' :: (Text, TossCall) -> Text
      title' _ = "Not Used"
   in RB.element <$> querySuite' (baseCfg RcAll) title' titleAndCall (mockSuite @FixedEffs)

baseCfg :: TossResult -> RunConfig
baseCfg = RunConfig "Unit Test Config"

demoFilter :: TossResult -> [AddressTxtElm (TestInfo TestConfig)]
demoFilter tr = toStrElm <$> fromRight' (queryFilterSuite (filters' Nothing) (baseCfg tr) (mockSuite @FixedEffs))

-- $ > view demoFilterAll
-- demoFilterAll :: [AddressTxtElm (TestInfo TestConfig)
demoFilterAll :: [AddressTxtElm (TestInfo TestConfig)]
demoFilterAll = demoFilter RcAll

chkTitles :: [Text] -> TossResult -> Assertion
chkTitles expected tossResult =   
  let 
    title'' :: AddressTxtElm (TestInfo TestConfig) -> Text
    title'' = (title :: TestInfo TestConfig -> Text) . el 
  in
   chkEq expected $ title'' <$> demoFilter tossResult

-- $ > unit_filter_all_has_all
unit_filter_all_has_all :: Assertion
unit_filter_all_has_all = chkTitles allMockTestTitles RcAll

allMockTestTitles :: [Text]
allMockTestTitles = ["test1HeadsTxt", "test4HeadsTxt", "test6HeadsTxt", "test5TailsInt", "test2TailsInt", "test3TailsInt", "test6HeadsTxt"]

heads :: [Text]
heads = ["test1HeadsTxt", "test4HeadsTxt", "test6HeadsTxt", "test6HeadsTxt"]

tails' :: [Text]
tails' = ["test5TailsInt", "test2TailsInt", "test3TailsInt"]

-- $ > unit_filter_heads_has_heads
unit_filter_heads_has_heads :: Assertion
unit_filter_heads_has_heads = chkTitles heads RcHeads

-- $ > unit_filter_tails_has_tails
unit_filter_tails_has_tails :: Assertion
unit_filter_tails_has_tails = chkTitles tails' RcTails

-- $ > view demoFilterHeads
demoFilterHeads :: [AddressTxtElm (TestInfo TestConfig)]
demoFilterHeads = demoFilter RcHeads

-- $ > view demoFilterTails
demoFilterTails :: [AddressTxtElm (TestInfo TestConfig)]
demoFilterTails = demoFilter RcTails


filterResults :: [TestFilter RunConfig TestConfig] -> RunConfig -> [TestFilterResult]
filterResults = filterLog (mockSuite @FixedEffs)

data Status = Accepted | Rejected | AnyResult deriving (Eq)

type ShowFilter = (Text, Maybe Text)

tests' :: RunConfig -> [TestFilter RunConfig TestConfig] -> Status -> [ShowFilter]
tests' rc fltrs s =
  let matchStatus :: (Text, Maybe Text) -> Bool
      matchStatus sf = s == AnyResult || (s == Accepted ? isNothing $ isJust) (snd sf)
      
      frslts ::  [TestFilterResult]
      frslts = filterResults  fltrs rc

   in P.filter matchStatus $ showIt <$> frslts

showIt :: TestFilterResult -> ShowFilter
showIt r = ((title :: TestLogInfo -> Text) $ testInfo r, reasonForRejection r)

filters' :: Maybe Text -> [TestFilter RunConfig TestConfig]
filters' ttl = [tossFilter, hasTitle ttl]


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

-- $ > view headsAccepted

headsAccepted ::  [ShowFilter]
headsAccepted = tests' (baseCfg RcHeads) (filters' Nothing) Accepted

-- $ > view headsWith6Rejects
headsWith6Rejects ::  [ShowFilter]
headsWith6Rejects = tests' (baseCfg RcHeads) (filters' $ Just "6") Rejected

-- $ > unit_check_rejection_messages

unit_check_rejection_messages :: Assertion
unit_check_rejection_messages =
  chkEq
    headsWith6Rejects
    [ ("test1HeadsTxt", Just "test title must include: 6"),
      ("test4HeadsTxt", Just "test title must include: 6"),
      ("test5TailsInt", Just "toss call: Tails must match run: RcHeads"),
      ("test2TailsInt", Just "toss call: Tails must match run: RcHeads"),
      ("test3TailsInt", Just "toss call: Tails must match run: RcHeads")
    ]
