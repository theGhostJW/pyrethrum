-- {-# LANGUAGE NoStrictData #-}

module SuiteRunIntegrityTest where

import Common (DetailedInfo (DetailedInfo), FrameworkError, HookType (..))
import DSL.Interpreter (effExecuteLog, minInterpret, executeInIOConsolePretty)
import DSL.LogProtocol (LogProtocolBase (..))
import DSL.LogProtocol.PrettyPrint (LogStyle (..), prettyPrintLogProtocol)
import DSL.Logger
import Data.Foldable (Foldable (length))
import Data.Text (Text)
import qualified Data.Text as Text
import EvalHelp
import GHC.Records
import ItemRunners (runItem)
import DemoSuite as M (
  ChannelSelect (WebOnly),
  DemoEffs,
  RunConfig (RunConfig),
  TestConfig (..),
  TextItem,
  everythingRun,
  demoRun,
  demoSuite,
  rcRunAll,
  channelFilter,
  webRun,
  restRun,
  txtRun)
import Polysemy
import Polysemy.Internal.Union (Member)
import Pyrelude as P
import Pyrelude.IO (putStrLn)
import Pyrelude.Test (Assertion, chk, chk', (...), chkEq)
import RunElementClasses as REC (Address (..), AddressElem (..), AddressTxtElm, TestLogInfo (..), toStrElm)
import qualified RunElementClasses as C
import Runner (SuiteSource, TestFilterResult (TestFilterResult, reasonForRejection, testInfo), config, title)
import RunnerBase as RB (AddressedElm (..), TestInfo, querySuite, testInfo)
import TempUtils
import TestFilter
import Text.Show.Pretty (pPrint, pPrintList, ppShowList)


{-
  These tests are just checking for the expected hooks and tests are run in the expected
  order under different filters
-}

-- mockSuit' :: SuiteSource Text TestConfig RunConfig FixedEffs a
-- mockSuit' = demoSuite @FixedEffs

-- -- $ > view demoQueryElem

-- demoQueryElem :: [AddressTxtElm (TestInfo TestConfig)]
-- demoQueryElem = toStrElm <$> querySuite rcRunAll (demoSuite @FixedEffs)


display :: (Show a1, Show a2) => Either a1 a2 -> IO ()
display eth = eitherf eth view view

-- $ > rslt *> view "Done"

showAll :: IO ()
showAll = effExecuteLog everythingRun >>= display

-- the test suite just logs messages
logMessages :: LogProtocolBase Text -> Maybe Text
logMessages = \case
  Message t -> Just t
  _ -> Nothing

-- $ > showSuiteMessages

showSuiteMessages :: IO (Either (FrameworkError Text) ([LogProtocolBase Text], ())) -> IO ()
showSuiteMessages sm = suitMessages sm >>= view

-- $ > suitMessages

suitMessages :: IO (Either (FrameworkError Text) ([LogProtocolBase Text], ())) -> IO [Text]
suitMessages rslt = fromRight' . (catMaybes . (logMessages <$>) . fst <$>) <$> rslt


-- expected suite messages will probably fail / need reworking 
-- when concurrency implemented
fullExpectedResult :: [Text]
fullExpectedResult =
  [ "BH - Group 1 >> Before Hook 1",
    "interact start",
    "test1WebTxt 1",
    "interact end",
    "interact start",
    "test1WebTxt 2",
    "interact end",
    "interact start",
    "test1WebTxt 3",
    "interact end",
    "interact start",
    "test1WebTxt 4",
    "interact end",
    "interact start",
    "test1WebTxt 5",
    "interact end",
    "BH - Group 1 >> Group 3 >> Before Hook 1.1",
    "interact start",
    "test61WebTxt 1",
    "interact end",
    "interact start",
    "test5RESTTxt 1",
    "interact end",
    "interact start",
    "test5RESTTxt 2",
    "interact end",
    "AH - Group 1 >> Group 3 >> After Hook 1.1",
    "AH - Group 1 >> After Hook 1",
    "BH - Group 1 >> Group 2 >> Before Hook 1",
    "interact start",
    "test2RESTInt 1",
    "interact end",
    "interact start",
    "test2RESTInt 2",
    "interact end",
    "AH - Group 1 >> Group 2 >> After Hook 1"
  ]

-- $> unit_tests_hooks_run_as_expected_full_suite
unit_tests_hooks_run_as_expected_full_suite :: IO ()
unit_tests_hooks_run_as_expected_full_suite =
  suitMessages (effExecuteLog everythingRun) >>= chkEq fullExpectedResult

-- expected suite messages will probably fail / need reworking 
-- when concurrency implemented
webResult :: [Text]
webResult =
  [  "BH - Group 1 >> Before Hook 1"
    , "interact start"
    , "test1WebTxt 1"
    , "interact end"
    , "interact start"
    , "test1WebTxt 2"
    , "interact end"
    , "interact start"
    , "test1WebTxt 3"
    , "interact end"
    , "interact start"
    , "test1WebTxt 4"
    , "interact end"
    , "interact start"
    , "test1WebTxt 5"
    , "interact end"
    , "BH - Group 1 >> Group 3 >> Before Hook 1.1"
    , "interact start"
    , "test61WebTxt 1"
    , "interact end"
    , "AH - Group 1 >> Group 3 >> After Hook 1.1"
    , "AH - Group 1 >> After Hook 1"
  ]

-- $> unit_tests_hooks_run_as_expected_web_suite
unit_tests_hooks_run_as_expected_web_suite :: IO ()
unit_tests_hooks_run_as_expected_web_suite =
  suitMessages (effExecuteLog webRun) >>= chkEq webResult


-- expected suite messages will probably fail / need reworking 
-- when concurrency implemented
restResult :: [Text]
restResult =
  [ "BH - Group 1 >> Before Hook 1",
    "BH - Group 1 >> Group 3 >> Before Hook 1.1",
    "interact start",
    "test5RESTTxt 1",
    "interact end",
    "interact start",
    "test5RESTTxt 2",
    "interact end",
    "AH - Group 1 >> Group 3 >> After Hook 1.1",
    "AH - Group 1 >> After Hook 1",
    "BH - Group 1 >> Group 2 >> Before Hook 1",
    "interact start",
    "test2RESTInt 1",
    "interact end",
    "interact start",
    "test2RESTInt 2",
    "interact end",
    "AH - Group 1 >> Group 2 >> After Hook 1"
  ]

-- $> unit_tests_hooks_run_as_expected_rest_suite
unit_tests_hooks_run_as_expected_rest_suite :: IO ()
unit_tests_hooks_run_as_expected_rest_suite =
  suitMessages (effExecuteLog restRun) >>= chkEq restResult

-- expected suite messages will probably fail / need reworking 
-- when concurrency implemented
txtExpectedResult :: [Text]
txtExpectedResult =
  [ "BH - Group 1 >> Before Hook 1",
    "interact start",
    "test1WebTxt 1",
    "interact end",
    "interact start",
    "test1WebTxt 2",
    "interact end",
    "interact start",
    "test1WebTxt 3",
    "interact end",
    "interact start",
    "test1WebTxt 4",
    "interact end",
    "interact start",
    "test1WebTxt 5",
    "interact end",
    "BH - Group 1 >> Group 3 >> Before Hook 1.1",
    "interact start",
    "test61WebTxt 1",
    "interact end",
    "interact start",
    "test5RESTTxt 1",
    "interact end",
    "interact start",
    "test5RESTTxt 2",
    "interact end",
    "AH - Group 1 >> Group 3 >> After Hook 1.1",
    "AH - Group 1 >> After Hook 1"
  ]


-- $> unit_tests_hooks_run_as_expected_text_suite
unit_tests_hooks_run_as_expected_text_suite :: IO ()
unit_tests_hooks_run_as_expected_text_suite =
  suitMessages (effExecuteLog txtRun) >>= chkEq txtExpectedResult

-- $ > consoleRunDemo
consoleRunDemo :: IO (Either (FrameworkError Text) ())
consoleRunDemo = executeInIOConsolePretty everythingRun




