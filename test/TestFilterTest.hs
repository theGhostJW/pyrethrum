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

-- $ > view allTossCalls

allTossCalls :: forall effs. DemoEffs effs => [(Text, TossCall)]
allTossCalls =
  let titleAndCall :: rc -> Address -> MockTest ho i as ds effs -> (Text, TossCall)
      titleAndCall _ _ (R.Test (TestConfig ttl call) _ _ _) = (ttl, call)

      title' :: (Text, TossCall) -> Text
      title' _ = "Not Used"
   in RB.element <$> querySuite' (baseCfg RcAll) title' titleAndCall mockSuite

baseCfg :: TossResult -> RunConfig
baseCfg = RunConfig "Unit Test Config"

demoFilter :: forall effs. DemoEffs effs => TossResult -> [AddressTxtElm (TestInfo TestConfig)]
demoFilter tr = toStrElm <$> fromRight' (queryFilterSuite (filters' Nothing) (baseCfg tr) (mockSuite @effs))

-- $ > view demoFilterAll
-- demoFilterAll :: [AddressTxtElm (TestInfo TestConfig)]

demoFilterAll :: forall effs. DemoEffs effs => [AddressTxtElm (TestInfo TestConfig)]
demoFilterAll = demoFilter @effs RcAll

chkTitles :: forall effs. DemoEffs effs => [Text] -> TossResult -> Assertion
chkTitles expected tossResult =   
  let 
    title'' :: AddressTxtElm (TestInfo TestConfig) -> Text
    title'' = (title :: TestInfo TestConfig -> Text) . el 
  in
   chkEq expected $ title'' <$> demoFilter @effs tossResult

-- $ > unit_filter_all_has_all
unit_filter_all_has_all :: forall effs. DemoEffs effs => Assertion
unit_filter_all_has_all = chkTitles @effs allMockTestTitles RcAll

allMockTestTitles :: [Text]
allMockTestTitles = ["test1HeadsTxt", "test4HeadsTxt", "test6HeadsTxt", "test5TailsInt", "test2TailsInt", "test3TailsInt", "test6HeadsTxt"]

heads :: [Text]
heads = ["test1HeadsTxt", "test4HeadsTxt", "test6HeadsTxt", "test6HeadsTxt"]

tails' :: [Text]
tails' = ["test5TailsInt", "test2TailsInt", "test3TailsInt"]

-- $ > unit_filter_heads_has_heads
unit_filter_heads_has_heads :: forall effs. DemoEffs effs => Assertion
unit_filter_heads_has_heads = chkTitles @effs heads RcHeads

-- $ > unit_filter_tails_has_tails
unit_filter_tails_has_tails :: forall effs. DemoEffs effs => Assertion
unit_filter_tails_has_tails = chkTitles @effs tails' RcTails

-- $ > view demoFilterHeads
-- demoFilterAll :: [AddressTxtElm (TestInfo TestConfig)]

demoFilterHeads :: forall effs. DemoEffs effs => [AddressTxtElm (TestInfo TestConfig)]
demoFilterHeads = demoFilter @effs RcHeads

-- $ > view demoFilterTails
-- demoFilterAll :: [AddressTxtElm (TestInfo TestConfig)]

demoFilterTails :: forall effs. DemoEffs effs => [AddressTxtElm (TestInfo TestConfig)]
demoFilterTails = demoFilter @effs RcTails


filterResults :: forall effs. DemoEffs effs =>  [TestFilter RunConfig TestConfig] -> RunConfig -> [TestFilterResult]
filterResults = filterLog (mockSuite @effs)

data Status = Accepted | Rejected | AnyResult deriving (Eq)

type ShowFilter = (Text, Maybe Text)

tests' :: forall effs. DemoEffs effs => RunConfig -> [TestFilter RunConfig TestConfig] -> Status -> [ShowFilter]
tests' rc fltrs s =
  let matchStatus :: (Text, Maybe Text) -> Bool
      matchStatus sf = s == AnyResult || (s == Accepted ? isNothing $ isJust) (snd sf)
      
      frslts ::  [TestFilterResult]
      frslts = (filterResults @effs) fltrs rc

   in P.filter matchStatus $ showIt <$> frslts

showIt :: TestFilterResult -> ShowFilter
showIt r = ((title :: TestLogInfo -> Text) $ testInfo r, reasonForRejection r)

filters' :: Maybe Text -> [TestFilter RunConfig TestConfig]
filters' ttl = [tossFilter, hasTitle ttl]

-- $ > view allTests

allTests :: forall effs. DemoEffs effs => [ShowFilter]
allTests = tests' @effs (baseCfg RcAll) (filters' Nothing) Accepted


{- $ >
import Pyrelude as P
unit_test_any_result_has_all :: forall effs. DemoEffs effs => IO ()
unit_test_any_result_has_all = chkEq (length (allTests @effs)) (length (headsAll @effs))
< $ -}

-- $> unit_test_any_result_has_all
unit_test_any_result_has_all :: forall effs. DemoEffs effs => Assertion
unit_test_any_result_has_all = chkEq (length (allTests @effs)) (length (headsAll @effs))

-- $ > view headsAll

headsAll :: forall effs. DemoEffs effs => [ShowFilter]
headsAll = tests' @effs (baseCfg RcHeads) (filters' Nothing) AnyResult

-- $ > view headsRejected

headsRejected :: forall effs. DemoEffs effs => [ShowFilter]
headsRejected = tests' @effs (baseCfg RcHeads) (filters' Nothing) Rejected

-- $ > view headsAccepted

headsAccepted :: forall effs. DemoEffs effs => [ShowFilter]
headsAccepted = tests' @effs (baseCfg RcHeads) (filters' Nothing) Accepted

-- $ > view headsWith6Rejects

headsWith6Rejects :: forall effs. DemoEffs effs => [ShowFilter]
headsWith6Rejects = tests' @effs (baseCfg RcHeads) (filters' $ Just "6") Rejected

-- $ > unit_check_rejection_messages

unit_check_rejection_messages :: forall effs. DemoEffs effs => Assertion
unit_check_rejection_messages =
  chkEq
    (headsWith6Rejects @effs)
    [ ("test1HeadsTxt", Just "test title must include: 6"),
      ("test4HeadsTxt", Just "test title must include: 6"),
      ("test5TailsInt", Just "toss call: Tails must match run: RcHeads"),
      ("test2TailsInt", Just "toss call: Tails must match run: RcHeads"),
      ("test3TailsInt", Just "toss call: Tails must match run: RcHeads")
    ]
