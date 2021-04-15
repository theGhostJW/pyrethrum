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
import DSL.LogProtocol.PrettyPrint
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
-- Right ([BoundaryLog (StartRun {runTitle = RunTitle {unRunTitle = "Happy Run"}, runUtcOffsetMins = 0, runConfig = Object (fromList [("include",Bool True),("cfgHeader",String "Happy Run")])}),BoundaryLog (FilterLog [TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test1"}, testTitle = "test1", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test1")])),("header",String "test1")])}, reasonForRejection = Nothing},TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test2 address"}, testTitle = "test2", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test2 address")])),("header",String "test2")])}, reasonForRejection = Nothing},TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test3 address"}, testTitle = "test3", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test3 address")])),("header",String "test3")])}, reasonForRejection = Nothing},TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test4 address"}, testTitle = "test4", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test4 address")])),("header",String "test4")])}, reasonForRejection = Nothing},TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test 5"}, testTitle = "test5", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test 5")])),("header",String "test5")])}, reasonForRejection = Nothing}]),BoundaryLog (StartGroup (GroupTitle {unGroupTitle = "Filter Suite"})),BoundaryLog (StartGroup (GroupTitle {unGroupTitle = "Sub Group"})),BoundaryLog (EndGroup (GroupTitle {unGroupTitle = "Sub Group"})),BoundaryLog (EndGroup (GroupTitle {unGroupTitle = "Filter Suite"})),BoundaryLog EndRun],())
happySuiteResult :: Either (FrameworkError Text) ([LogProtocolBase Text], ())
happySuiteResult = minInterpret happyRun

unit_happy_suit_passes_validation :: Assertion
unit_happy_suit_passes_validation = chk $ isRight $ debug happySuiteResult

-- >>> hookRunResult
-- [BoundaryLog (StartRun {runTitle = RunTitle {unRunTitle = "Happy Run"}, runUtcOffsetMins = 0, runConfig = Object (fromList [("include",Bool True),("cfgHeader",String "Happy Run")])}),BoundaryLog (FilterLog [TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test1"}, testTitle = "test1", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test1")])),("header",String "test1")])}, reasonForRejection = Nothing}]),BoundaryLog (StartGroup (GroupTitle {unGroupTitle = "Hook Suite"})),BoundaryLog (EndGroup (GroupTitle {unGroupTitle = "Hook Suite"})),BoundaryLog EndRun]
hookRunResult :: [LogProtocolTextError]
hookRunResult = fst . fromRight' $ minInterpret hookRun

-- >>> hookResultPretty
-- [BoundaryLog (StartRun {runTitle = RunTitle {unRunTitle = "Hook Suite"}, runUtcOffsetMins = 0, runConfig = Object (fromList [("include",Bool True),("cfgHeader",String "Hook Suite")])}),BoundaryLog (FilterLog [TestFilterResult {testInfo = TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test1"}, testTitle = "test1", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test1")])),("header",String "test1")])}, reasonForRejection = Nothing}]),BoundaryLog (StartGroup (GroupTitle {unGroupTitle = "Hook Suite"})),IterationLog (Doc (DocMessage "Before All Hook")),IterationLog (Doc (DocMessage "Before Each Hook")),BoundaryLog (StartTest (TestDisplayInfo {testModAddress = TestAddress {unTestAddress = "test1"}, testTitle = "test1", testConfig = Object (fromList [("include",Bool True),("address",Object (fromList [("unTestAddress",String "test1")])),("header",String "test1")])})),BoundaryLog (StartIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}) (WhenClause {unWhenClause = "pre"}) (ThenClause {unThenClause = "post"}) (Number 1.0)),IterationLog (Run StartInteraction),IterationLog (Doc (DocMessage "interact start")),IterationLog (Doc (DocMessage "Hook Suite - Hello from item 1")),IterationLog (Doc (DocMessage "interact end")),IterationLog (Run (InteractorSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}) (ApStateJSON {unApStateJSON = String "test1: "}))),IterationLog (Run StartParser),IterationLog (Run (ParserSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1}) (DStateJSON {unDStateJSON = String "\"test1: \""}))),IterationLog (Run StartChecks),BoundaryLog (EndIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 1})),IterationLog (Doc (DocMessage "Before Each Hook")),BoundaryLog (StartIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}) (WhenClause {unWhenClause = "pre"}) (ThenClause {unThenClause = "post"}) (Number 2.0)),IterationLog (Run StartInteraction),IterationLog (Doc (DocMessage "interact start")),IterationLog (Doc (DocMessage "Hook Suite - Hello from item 2")),IterationLog (Doc (DocMessage "interact end")),IterationLog (Run (InteractorSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}) (ApStateJSON {unApStateJSON = String "test1: "}))),IterationLog (Run StartParser),IterationLog (Run (ParserSuccess (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2}) (DStateJSON {unDStateJSON = String "\"test1: \""}))),IterationLog (Run StartChecks),BoundaryLog (EndIteration (ItemId {tstModule = TestAddress {unTestAddress = "test1"}, itmId = 2})),BoundaryLog (EndTest (TestAddress {unTestAddress = "test1"})),BoundaryLog (EndGroup (GroupTitle {unGroupTitle = "Hook Suite"})),BoundaryLog EndRun]
hookResultPretty = hookRunResult

msgTxt :: LogProtocolTextError -> Maybe Text 
msgTxt = \case 
                BoundaryLog _ -> Nothing  
                rp -> case rp of 
                    Message txt -> Just txt
                    Message' (DetailedInfo m i) -> Just $ m <> " - " <> i 
                    _ -> Nothing  

msgPredicate :: (Text -> Bool) -> LogProtocolTextError -> Bool 
msgPredicate prd = \case 
                      BoundaryLog _ -> False 
                      rp -> case rp of 
                          Message txt -> prd txt
                          Message' (DetailedInfo m i) -> prd m || prd i 
                          _ -> False 

msgContains :: Text -> LogProtocolTextError -> Bool 
msgContains txt = msgPredicate (isInfixOf txt)
