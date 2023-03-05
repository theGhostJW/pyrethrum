module TestFilterTest where

import Check (Checks)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import DemoSuite hiding (filters')
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
allTossCalls :: [(Text, Channel)]
allTossCalls =
  let titleAndCall :: rc -> Address -> DemoTest ho i as ds effs -> (Text, Channel)
      titleAndCall _ _ (R.Test (TestConfig ttl call) _ _ _) = (ttl, call)

      title' :: (Text, Channel) -> Text
      title' _ = "Not Used"
   in RB.element <$> querySuite' (baseCfg AllChannels) title' titleAndCall (demoSuite @FixedEffs)

baseCfg :: ChannelSelect -> RunConfig
baseCfg = RunConfig "Unit Test Config"

demoFilter :: ChannelSelect -> [AddressTxtElm (TestInfo TestConfig)]
demoFilter tr = toStrElm <$> fromRight' (queryFilterSuite (filters' Nothing) (baseCfg tr) (demoSuite @FixedEffs))

-- $ > view showFilters
showFilters :: Either Text FilterLog
showFilters = filterSuite (baseCfg AllChannels)  (demoSuite @FixedEffs) (filters' Nothing)



-- $ > view demoFilterAll
-- demoFilterAll :: [AddressTxtElm (TestInfo TestConfig)
demoFilterAll :: [AddressTxtElm (TestInfo TestConfig)]
demoFilterAll = demoFilter AllChannels

chkTitles :: [Text] -> ChannelSelect -> Assertion
chkTitles expected tossResult =   
  let 
    title'' :: AddressTxtElm (TestInfo TestConfig) -> Text
    title'' = getField @"title" . el 
  in
   chkEq expected $ title'' <$> demoFilter tossResult

-- $ > unit_filter_all_has_all
unit_filter_all_has_all :: Assertion
unit_filter_all_has_all = chkTitles allDemoTestTitles AllChannels

allDemoTestTitles :: [Text]
allDemoTestTitles = [
                      "test1WebTxt", 
                      -- "test4WebTxt", no test items
                      "test61WebTxt", 
                      "test5RESTTxt", 
                      "test2RESTInt" --, 
                      -- "test3RESTInt", no test items
                      -- "test6WebTxt" no test items
                      ]

webTests :: [Text]
webTests = ["test1WebTxt", 
            --  "test4WebTxt", no test items
            "test61WebTxt"]

rest' :: [Text]
rest' = [
        "test5RESTTxt", 
        "test2RESTInt" 
        -- "test3RESTInt" no test items
        ]

-- $ > unit_filter_Web_has_Web
unit_filter_Web_has_Web :: Assertion
unit_filter_Web_has_Web = chkTitles webTests WebOnly

-- $ > unit_filter_REST_has_REST
unit_filter_REST_has_REST :: Assertion
unit_filter_REST_has_REST = chkTitles rest' RESTOnly

-- $ > view demoFilterWeb
demoFilterWeb :: [AddressTxtElm (TestInfo TestConfig)]
demoFilterWeb = demoFilter WebOnly

-- $ > view demoFilterREST
demoFilterREST :: [AddressTxtElm (TestInfo TestConfig)]
demoFilterREST = demoFilter RESTOnly


filterResults :: [TestFilter RunConfig TestConfig] -> RunConfig -> [TestFilterResult]
filterResults = filterLog (demoSuite @FixedEffs)

data Status = Accepted | Rejected | AnyResult deriving (Eq)

type ShowFilter = (Text, Maybe Text)

testFilterSummary :: RunConfig -> [TestFilter RunConfig TestConfig] -> Status -> [ShowFilter]
testFilterSummary rc fltrs s =
  let matchStatus :: (Text, Maybe Text) -> Bool
      matchStatus sf = s == AnyResult || (s == Accepted ? isNothing $ isJust) (snd sf)
      
      frslts ::  [TestFilterResult]
      frslts = filterResults fltrs rc

   in P.filter matchStatus $ showIt <$> frslts

showIt :: TestFilterResult -> ShowFilter
showIt r = (getField @"title" $ testInfo r, reasonForRejection r)

filters' :: Maybe Text -> [TestFilter RunConfig TestConfig]
filters' ttl = [channelFilter, hasTitle ttl]


-- $ > view allTests
allTests :: [ShowFilter]
allTests = testFilterSummary (baseCfg AllChannels) (filters' Nothing) Accepted


-- $ > view webAll

webAll :: [ShowFilter]
webAll = testFilterSummary (baseCfg WebOnly) (filters' Nothing) AnyResult

-- $ > view webRejected
webRejected :: [ShowFilter]
webRejected = testFilterSummary (baseCfg WebOnly) (filters' Nothing) Rejected

-- $ > view webAccepted

webAccepted ::  [ShowFilter]
webAccepted = testFilterSummary (baseCfg WebOnly) (filters' Nothing) Accepted

-- $ > view webWith6Rejects
webWith6Rejects ::  [ShowFilter]
webWith6Rejects = testFilterSummary (baseCfg WebOnly) (filters' $ Just "6") Rejected

-- $ > unit_check_rejection_messages

unit_check_rejection_messages :: Assertion
unit_check_rejection_messages =
  chkEq
    webWith6Rejects
    [ ("test1WebTxt", Just "test title must include: 6"),
      ("test4WebTxt", Just "Empty test items - Test items are empty under this run configuration"),
      ("test5RESTTxt", Just "channel: REST must match run: WebOnly"),
      ("test2RESTInt", Just "channel: REST must match run: WebOnly"),
      ("test3RESTInt", Just "Empty test items - Test items are empty under this run configuration"),
      ( "test6WebTxt", Just "Empty test items - Test items are empty under this run configuration")
    ]
