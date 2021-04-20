module SuiteValidationTest where

import MockSuite ( happyRun, MyText, happySuite, demoSuit, hookRun, LogProtocolTextError)
import DSL.Interpreter ( minInterpret )
import Pyrelude as P
import Pyrelude.Test ( chk, Assertion, (...) )
import DSL.LogProtocol ( LogProtocolBase (..))
import Common  ( FrameworkError, DetailedInfo(DetailedInfo) )
import Runner (groupAddresses)
import TempUtils
import ItemRunners (runItem)
import Data.Foldable (Foldable(length))
import Data.Text ( Text )
import DSL.LogProtocol.PrettyPrint (prettyPrintLogProtocol, LogStyle(..))
import qualified Data.Text as Text


expectedDemoGroupNames :: [Text]
expectedDemoGroupNames = ["Happy Suite", "Happy Suite.Sub Group", "Happy Suite.Empty Group"] 

unit_demo_group_addresses_count :: Assertion
unit_demo_group_addresses_count = 
  P.length expectedDemoGroupNames ... P.length $ groupAddresses demoSuit

unit_demo_group_addresses :: Assertion
unit_demo_group_addresses = 
  expectedDemoGroupNames ... groupAddresses demoSuit

happySuiteResult :: Either (FrameworkError Text) ([LogProtocolBase Text], ())
happySuiteResult = minInterpret happyRun

-- $ > unit_happy_suit_passes_validation
unit_happy_suit_passes_validation :: Assertion
unit_happy_suit_passes_validation = chk $ isRight happySuiteResult


hookRunResult :: [LogProtocolTextError]
hookRunResult = fst . fromRight' $ minInterpret hookRun

-- $> hookResultPretty
hookResultPretty :: IO ()
hookResultPretty = debugLines $ prettyPrintLogProtocol Outline <$> hookRunResult

msgTxt :: LogProtocolTextError -> Maybe Text 
msgTxt = \case 
            Message tx -> Just tx
            Message' (DetailedInfo m i) -> Just $ m <> " - " <> i 
            _ -> Nothing  

msgPredicate :: (Text -> Bool) -> LogProtocolTextError -> Bool 
msgPredicate prd = \case 
                      Message tx -> prd tx
                      Message' (DetailedInfo m i) -> prd m || prd i 
                      _ -> False 

msgContains :: Text -> LogProtocolTextError -> Bool 
msgContains tx = msgPredicate (isInfixOf tx)
