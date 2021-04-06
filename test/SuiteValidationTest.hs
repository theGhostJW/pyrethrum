module SuiteValidationTest where

import MockSuite ( happyRun, MyText, happySuite, demoSuit, hookRun)
import DSL.Interpreter ( minInterpret )
import Pyrelude ( ($), Either, isRight, debug, fromRight', toS )
import Pyrelude.Test ( chk, Assertion, (...) )
import DSL.LogProtocol ( LogProtocolBase )
import Common ( FrameworkError )
import Runner (groupAddresses)
import ItemRunners (runItem)
import Data.Foldable (Foldable(length))
import Data.Text ( Text )
import Prelude as Eval
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
hookRunResult :: [LogProtocolBase Text]
hookRunResult = fst . fromRight' $ minInterpret hookRun

-- >>> hookResultPretty
-- ["################################################################################\n################################## Happy Run ###################################\n################################################################################\n\n\nRun Config:\n  include: true\n  cfgHeader: Happy Run","\n==== Filter Log ====\naccepted: test1 - test1\n","==== Group - Hook Suite ====","  message: Before All Hook","\n=== Start Test: test1 - test1 ===\nTest Config:\n  include: true\n  address:\n    unTestAddress: test1\n  header: test1","\n---- Start Iteration: test1 / item -999 ----\nItem:\n  1\n","\nInteraction:","  message: interact start","  message: Happy Run Hello from item 1","  message: interact end","\n      > Interactor Complete - test1 / item -999\n      'test1: '","\nDomain:","      > ParseComplete - test1 / item -999\n      '\"test1: \"'","\nChecks:","\n---- End Iteration: test1 / item -999 ----","\n---- Start Iteration: test1 / item -999 ----\nItem:\n  2\n","\nInteraction:","  message: interact start","  message: Happy Run Hello from item 2","  message: interact end","\n      > Interactor Complete - test1 / item -999\n      'test1: '","\nDomain:","      > ParseComplete - test1 / item -999\n      '\"test1: \"'","\nChecks:","\n---- End Iteration: test1 / item -999 ----","\n=== End Test: test1 ===","==== End Group - Hook Suite ====","\n==== End Run ===="]
hookResultPretty :: [Text]
hookResultPretty = prettyPrintLogProtocol False <$> hookRunResult
