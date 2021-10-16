-- {-# LANGUAGE NoStrictData #-}

module SuiteValidationTest where

import Common (DetailedInfo (DetailedInfo), FrameworkError, HookType (..))
import DSL.Interpreter (minInterpret)
import DSL.LogProtocol (LogProtocolBase (..))
import DSL.LogProtocol.PrettyPrint (LogStyle (..), prettyPrintLogProtocol)
import Data.Foldable (Foldable (length))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Records
import ItemRunners (runItem)
import MockSuite as M (MockTest, RunConfig (RunConfig), TestConfig (..), TextItem, happyRun, mockSuite, rcRunAll, tossFilter, TossResult (RcHeads))
import Polysemy
import Pyrelude as P
import Pyrelude.IO (putStrLn)
import Pyrelude.Test (Assertion, chk, chk', (...))
import RunElementClasses as REC (AddresStringElm, Address (..), AddressElem (..), TestLogInfo (..), toStrElm)
import Runner (TestFilterResult (TestFilterResult, reasonForRejection, testInfo), config, title)
import RunnerBase as RB (AddressedElm (..), TestInfo, querySuite, testInfo) 
import qualified RunElementClasses as C
import TempUtils
import TestFilter
import Text.Show.Pretty (pPrint, pPrintList, ppShowList)

view' :: Show a => [a] -> IO ()
view' = pPrintList

-- $ > view' demoQueryElem

demoQueryElem :: [AddresStringElm (TestInfo TestConfig)]
demoQueryElem = toStrElm <$> querySuite rcRunAll mockSuite

applyFilterLog :: TestFilter RunConfig TestConfig -> RunConfig -> [TestFilterResult]
applyFilterLog fltr = filterLog mockSuite [fltr]

listTests :: TestFilter RunConfig TestConfig -> RunConfig -> [Text]
listTests fltr rc =
  headDef "" . ((title :: AddressElem -> Text) <$>) . unAddress . (address :: TestLogInfo -> Address) . C.testInfo <$> filter (isNothing . reasonForRejection) (applyFilterLog fltr rc)

-- $ > expectedDemoGroupNames
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
  let fails = filter (\(f, s) -> p1 f && not (p2 s)) $ offsetList l i
      msg (f, s) = "target element: " <> txt f <> "\n" <> "element " <> txt i <> " after:\n" <> txt s
   in chk' (unlines $ msg <$> fails) (null fails)

chkNext :: Show t => (t -> Bool) -> (t -> Bool) -> [t] -> IO ()
chkNext = chkOffsetList 1

-- isEndHook :: HookType -> LogProtocolWithTextError -> Bool
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

-- isStartHook :: HookType -> LogProtocolWithTextError -> Bool
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
