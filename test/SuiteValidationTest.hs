module SuiteValidationTest where

import MockSuite ( happyRun, MyText, happySuite, demoSuit, hookRun, LogProtocolTextError)
import DSL.Interpreter ( minInterpret )
import Pyrelude ( ($), Either, isRight, debug, fromRight', toS, Listy (isInfixOf), catMaybes )
import Pyrelude.Test ( chk, Assertion, (...) )
import DSL.LogProtocol ( LogProtocolBase (..))
import Common  ( FrameworkError, DetailedInfo, DetailedInfo(DetailedInfo) )
import Runner (groupAddresses)
import ItemRunners (runItem)
import Data.Foldable (Foldable(length))
import Data.Text ( Text )
import Prelude
import DSL.LogProtocol.PrettyPrint (prettyPrintLogProtocol, prettyPrintLogProtocolSimple)
import qualified Data.Text as Text


expectedDemoGroupNames :: [Text]
expectedDemoGroupNames = ["Happy Suite", "Happy Suite.Sub Group", "Happy Suite.Empty Group"] 

unit_demo_group_addresses_count :: Assertion
unit_demo_group_addresses_count = 
  length expectedDemoGroupNames ... length $ groupAddresses demoSuit

unit_demo_group_addresses :: Assertion
unit_demo_group_addresses = 
  expectedDemoGroupNames ... groupAddresses demoSuit

-- >>> happySuiteResult
-- Right ([StartRun {runTitle = RunTitle {unRunTitle = "Happy Suite"}, runUtcOffsetMins = 0, runConfig = Object (fromList [("include",Bool True),("cfgHeader",String "Happy Suite")])},FilterLog [TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test1"}, testTitle = "test1", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test1")])),("header",String "test1")])}, reasonForRejection = Nothing},TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test2 address"}, testTitle = "test2", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test2 address")])),("header",String "test2")])}, reasonForRejection = Nothing},TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test3 address"}, testTitle = "test3", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test3 address")])),("header",String "test3")])}, reasonForRejection = Nothing},TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test4 address"}, testTitle = "test4", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test4 address")])),("header",String "test4")])}, reasonForRejection = Nothing},TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test 5"}, testTitle = "test5", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test 5")])),("header",String "test5")])}, reasonForRejection = Nothing}],StartGroup (GroupTitle {unGroupTitle = "Filter Suite"}),StartTest (TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test1"}, testTitle = "test1", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test1")])),("header",String "test1")])}),StartIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}) (WhenClause {unWhenClause = "pre"}) (ThenClause {unThenClause = "post"}) (Number 1.0),StartInteraction,Message "interact start",Message "Happy Suite - Hello from item 1",Message "interact end",InteractorSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}) (ApStateJSON {unApStateJSON = String "test1: "}),StartParser,ParserSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}) (DStateJSON {unDStateJSON = String "\"test1: \""}),StartChecks,EndIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}),StartIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}) (WhenClause {unWhenClause = "pre"}) (ThenClause {unThenClause = "post"}) (Number 2.0),StartInteraction,Message "interact start",Message "Happy Suite - Hello from item 2",Message "interact end",InteractorSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}) (ApStateJSON {unApStateJSON = String "test1: "}),StartParser,ParserSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}) (DStateJSON {unDStateJSON = String "\"test1: \""}),StartChecks,EndIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}),EndTest (TestAddress {unTestAddress = "test1"}),StartGroup (GroupTitle {unGroupTitle = "Sub Group"}),EndGroup (GroupTitle {unGroupTitle = "Sub Group"}),EndGroup (GroupTitle {unGroupTitle = "Filter Suite"}),EndRun],())
happySuiteResult :: Either (FrameworkError Text) ([LogProtocolBase Text], ())
happySuiteResult = minInterpret happyRun

unit_happy_suit_passes_validation :: Assertion
unit_happy_suit_passes_validation = chk $ isRight $ debug happySuiteResult

-- >>> hookRunResult
-- [StartRun {runTitle = RunTitle {unRunTitle = "Hook Suite"}, runUtcOffsetMins = 0, runConfig = Object (fromList [("include",Bool True),("cfgHeader",String "Hook Suite")])},FilterLog [TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test1"}, testTitle = "test1", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test1")])),("header",String "test1")])}, reasonForRejection = Nothing}],StartGroup (GroupTitle {unGroupTitle = "Hook Suite"}),Message "Before All Hook",Message "Before Each Hook",StartTest (TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test1"}, testTitle = "test1", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test1")])),("header",String "test1")])}),StartIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}) (WhenClause {unWhenClause = "pre"}) (ThenClause {unThenClause = "post"}) (Number 1.0),StartInteraction,Message "interact start",Message "Hook Suite - Hello from item 1",Message "interact end",InteractorSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}) (ApStateJSON {unApStateJSON = String "test1: "}),StartParser,ParserSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}) (DStateJSON {unDStateJSON = String "\"test1: \""}),StartChecks,EndIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}),Message "Before Each Hook",StartIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}) (WhenClause {unWhenClause = "pre"}) (ThenClause {unThenClause = "post"}) (Number 2.0),StartInteraction,Message "interact start",Message "Hook Suite - Hello from item 2",Message "interact end",InteractorSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}) (ApStateJSON {unApStateJSON = String "test1: "}),StartParser,ParserSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}) (DStateJSON {unDStateJSON = String "\"test1: \""}),StartChecks,EndIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}),EndTest (TestAddress {unTestAddress = "test1"}),EndGroup (GroupTitle {unGroupTitle = "Hook Suite"}),EndRun]
hookRunResult :: [LogProtocolTextError]
hookRunResult = fst . fromRight' $ minInterpret hookRun

-- >>> hookResultPretty
-- ["################################################################################\n################################## Hook Suite ##################################\n################################################################################\n\n\nRun Config:\n  include: true\n  cfgHeader: Hook Suite","\n==== Filter Log ====\naccepted: test1 - test1\n","==== Group - Hook Suite ====","  message: Before All Hook","  message: Before Each Hook"," @@@@ === Start Test: test1 - test1 === @@@@ Test Config: @@@@   include: true\n  address:\n    unTestAddress: test1\n  header: test1"," @@@@ ---- Start Iteration: test1 / item 1 ---- @@@@ Item: @@@@   1 @@@@ "," @@@@ Interaction:","  message: interact start","  message: Hook Suite - Hello from item 1","  message: interact end"," @@@@       > Interactor Complete - test1 / item 1 @@@@     'test1: '"," @@@@ Domain:","      > ParseComplete - test1 / item 1 @@@@     '\"test1: \"'"," @@@@ Checks:"," @@@@ ---- End Iteration: test1 / item 1 ----","  message: Before Each Hook"," @@@@ ---- Start Iteration: test1 / item 2 ---- @@@@ Item: @@@@   2 @@@@ "," @@@@ Interaction:","  message: interact start","  message: Hook Suite - Hello from item 2","  message: interact end"," @@@@       > Interactor Complete - test1 / item 2 @@@@     'test1: '"," @@@@ Domain:","      > ParseComplete - test1 / item 2 @@@@     '\"test1: \"'"," @@@@ Checks:"," @@@@ ---- End Iteration: test1 / item 2 ----"," @@@@ === End Test: test1 ===","==== End Group - Hook Suite ===="," @@@@ ==== End Run ===="]
hookResultPretty :: [Text]
hookResultPretty = prettyPrintLogProtocolSimple <$> hookRunResult

msgTxt :: LogProtocolTextError -> Maybe Text 
msgTxt = \case 
            Message txt -> Just txt
            Message' (DetailedInfo m i) -> Just $ m <> " - " <> i 
            _ -> Nothing  

msgPredicate :: (Text -> Bool) -> LogProtocolTextError -> Bool 
msgPredicate prd = \case 
                      Message txt -> prd txt
                      Message' (DetailedInfo m i) -> prd m || prd i 
                      _ -> False 

msgContains :: Text -> LogProtocolTextError -> Bool 
msgContains txt = msgPredicate (isInfixOf txt)
