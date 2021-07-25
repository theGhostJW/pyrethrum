-- {-# LANGUAGE NoStrictData #-} 


module SuiteValidationTest where

import MockSuite ( happyRun, MyText, MockTest, mockSuite, inOutFilter, inFilterRunConfig, TestConfig (TestConfig), RunConfig (RunConfig), outOfFilterRunConfig)
import DSL.Interpreter ( minInterpret )
import Pyrelude as P
import Pyrelude.Test ( chk, chk', Assertion, (...) )
import DSL.LogProtocol ( LogProtocolBase (..))
import Common  ( FrameworkError, DetailedInfo(DetailedInfo), HookCardinality(..) )
import Runner (groupAddresses, Titled (title), config, TestFilterResult (TestFilterResult, testInfo, reasonForRejection), TestDisplayInfo (testModAddress), TestAddress (unTestAddress))
import RunnerBase (querySuite, AddressedElm (AddressedElm, element))
import TempUtils
import ItemRunners (runItem)
import Data.Foldable (Foldable(length))
import Data.Text ( Text )
import DSL.LogProtocol.PrettyPrint (prettyPrintLogProtocol, LogStyle(..))
import qualified Data.Text as Text
import TestFilter

-- >>> demoQueryElem
-- [AddressedElm {address = Stack 2 ["test1","Filter TestSuite"], element = "test1"},AddressedElm {address = Stack 2 ["test4","Filter TestSuite"], element = "test4"},AddressedElm {address = Stack 3 ["test5","Nested Int Group","Filter TestSuite"], element = "test5"},AddressedElm {address = Stack 3 ["test2","Nested Int Group","Filter TestSuite"], element = "test2"}]
-- 
demoQueryElem :: [AddressedElm Text]
demoQueryElem =
  let
    getTitle :: a -> MockTest hi i as ds effs -> Text
    getTitle _ mt = Runner.title $ config mt

    root = mockSuite getTitle
  in
    querySuite id root

applyFilterLog :: TestFilter RunConfig TestConfig -> RunConfig -> [RunnerBase.AddressedElm TestFilterResult]
applyFilterLog fltr = filterLog mockSuite [fltr]

listTests :: TestFilter RunConfig TestConfig -> RunConfig -> [Text]
listTests fltr rc =
   unTestAddress . testModAddress . testInfo . element <$> filter (isNothing . reasonForRejection . element) (applyFilterLog fltr rc)

-- $> inFilterTests
inFilterTests :: [Text]
inFilterTests = listTests inOutFilter inFilterRunConfig

-- $> outFilterTests
outFilterTests :: [Text]
outFilterTests = listTests inOutFilter outOfFilterRunConfig

-- todo filter out empty items


expectedDemoGroupNames :: [Text]
expectedDemoGroupNames = ["Happy TestSuite", "Happy TestSuite.Sub Group", "Happy TestSuite.Empty Group"]

-- unit_demo_group_addresses_count :: Assertion
-- unit_demo_group_addresses_count = 
--   P.length expectedDemoGroupNames ... P.length $ groupAddresses demoSuit

-- unit_demo_group_addresses :: Assertion
-- unit_demo_group_addresses = 
--   expectedDemoGroupNames ... groupAddresses demoSuit

-- happySuiteResult :: Either (FrameworkError Text) ([LogProtocolBase Text], ())
-- happySuiteResult = minInterpret happyRun

-- $ > unit_happy_suit_passes_validation
-- unit_happy_suit_passes_validation :: Assertion
-- unit_happy_suit_passes_validation = chk $ isRight happySuiteResult


-- hookRunResult :: [LogProtocolWithTextError]
-- hookRunResult = fst . fromRight' $ minInterpret hookRun

-- -- $ > hookResultPretty
-- hookResultPretty :: IO ()
-- hookResultPretty = debugLines $ prettyPrintLogProtocol Outline <$> hookRunResult

offsetList :: [a] -> Int -> [(a, a)]
offsetList l i = zip l $ drop i l

chkOffsetList :: Show t => Int -> (t -> Bool) -> (t -> Bool) -> [t] -> IO ()
chkOffsetList i p1 p2 l =
  let
    fails = filter (\(f, s) -> p1 f && not (p2 s)) $ offsetList l i
    msg (f, s) = "target element: " <> txt f <> "\n" <> "element " <> txt i <> " after:\n" <> txt s
  in
    chk' (unlines $ msg <$> fails) (null fails)

chkNext :: Show t => (t -> Bool) -> (t -> Bool) -> [t] -> IO ()
chkNext = chkOffsetList 1

-- isEndHook :: HookCardinality -> LogProtocolWithTextError -> Bool
-- isEndHook hl = \case 
--                   EndHook loc _ -> loc == hl
--                   _ -> False

-- isStartIteration :: LogProtocolWithTextError -> Bool
-- isStartIteration = \case 
--                       StartIteration {} -> True
--                       _ -> False

-- $ > unit_before_each_hook_precedes_each_iteration TODO
-- unit_before_each_hook_precedes_each_iteration :: IO ()
-- unit_before_each_hook_precedes_each_iteration = chkNext (isEndHook BeforeEach) isStartIteration hookRunResult

-- isStartHook :: HookCardinality -> LogProtocolWithTextError -> Bool
-- isStartHook hl = \case 
--                   StartHook loc _ -> loc == hl
--                   _ -> False

-- isEndIteration :: LogProtocolWithTextError -> Bool
-- isEndIteration = \case 
--                       EndIteration _ -> True
--                       _ -> False

-- $ > unit_after_each_hook_follows_each_interaction TODO
-- unit_after_each_hook_follows_each_interaction :: IO ()
-- unit_after_each_hook_follows_each_interaction = chkNext isEndIteration (isStartHook AfterEach) hookRunResult
