module LogProtocolTest where

import           Foundation.Extended as F
import           Test.Extended as T
import qualified Prelude             as P
import DSL.LogProtocol
import Data.Functor.Identity
import Data.Functor
import DemoProject.Config
import TestAndRunConfig
import Data.Set as S
import DSL.Common
import TestFilter
import RunnerBase
import Data.Aeson hiding (Error)
import Data.ByteString.Lazy as B
import Hedgehog.Internal.Property
import Hedgehog.Internal.Property
import Control.Monad.IO.Class
import Data.Traversable
import Data.Char
import Data.Aeson.TH

genStr :: Gen String
genStr = 
  let 
    useVal :: String -> Bool
    useVal s = not $ F.elem s ["", "\NUL", "\SOH",  "\STX", "\ETX", "\EOT"]
  in
    T.filter useVal $ string (linear 0 1000) ascii


genTestConfig :: Gen TestConfig
genTestConfig =
  let
    set' :: (Enum a, Ord a) => Gen (Set a)
    set' = (S.fromList <$>) <$> subsequence $ enumList 
  in
    TestConfig 
      <$> genStr 
      <*> (TestModule <$> genStr)
      <*> set'
      <*> set'
      <*> element enumList
      <*> T.bool

genRunConfig :: Gen RunConfig
genRunConfig =
    RunConfig 
      <$> genStr 
      <*> element enumList
      <*> element enumList
      <*> element enumList

genDetailedInfo:: Gen DetailedInfo
genDetailedInfo = DetailedInfo <$> genStr <*> genStr

genTestDisplayInfo:: Gen (TestDisplayInfo TestConfig)
genTestDisplayInfo = TestDisplayInfo 
                                <$> (TestModule <$> genStr) 
                                <*> genStr 
                                <*> genTestConfig

genFilterResult:: Gen (FilterResult TestConfig)
genFilterResult = FilterResult 
                            <$> genTestDisplayInfo
                            <*> T.maybe genStr

genFilterResults :: Gen [(FilterResult TestConfig)]
genFilterResults =
         list (linear 0 20) genFilterResult

genLPrc :: Gen (LogProtocol RunConfig)
genLPrc = choice [
                   StartRun <$> genStr <*> genRunConfig,
                   EndRun <$> genRunConfig
                 ]

genLPStringInt :: Gen (LogProtocol (String, Int))
genLPStringInt = StartIteration <$> (TestModule <$> genStr) <*> int (linear 0 1000)

genLPStringIntString :: Gen (LogProtocol (String, Int, String))
genLPStringIntString = EndIteration <$> (TestModule <$> genStr) <*> int (linear 0 1000) <*> genStr

genLPString:: Gen (LogProtocol String)
genLPString =
  choice $ (genStr <&>) <$> [
    Message,
    Warning,
    IOAction,
    StartGroup
  ]

genLPDetailedInfo:: Gen (LogProtocol DetailedInfo)
genLPDetailedInfo = choice $ (genDetailedInfo <&>) <$> [Message', Warning']

genLPError:: Gen (LogProtocol AppError)
genLPError = (Error . AppUserError) <$> genStr

genLPTestConfig:: Gen (LogProtocol TestConfig)
genLPTestConfig = choice [
                          StartTest <$> genTestDisplayInfo,
                          FilterLog <$> genFilterResults
                         ]

lpRoundTrip :: Gen (LogProtocol a) -> (LogProtocol a -> Some LogProtocol -> PropertyT IO ()) -> Property
lpRoundTrip g chkFunc = property $ do
  lp <- forAll g
  let 
    serialised :: B.ByteString
    serialised = --debug' "Serialised" $ 
                 encode 
                -- $ debug' "Log Protocol Obj" 
                 $ Some lp

    unserialised :: Either P.String (Some LogProtocol)
    unserialised = -- debug' "Deserialised" $ 
                    eitherDecode serialised

  eitherf unserialised
    (\s -> (footnote s) *> failure)
    (chkFunc {- $ debug -} lp)

checkIt :: IO [LogProtocol String]
checkIt = traverse (\_ -> sample genLPString) [0..50]

hprop_log_protocol_string_round_trip  :: Property
hprop_log_protocol_string_round_trip  =
  let 
    checker :: LogProtocol String -> Some LogProtocol -> PropertyT IO ()
    checker lp slp = 
                case slp of
                    Some m@(Message _) -> lp === m
                    Some (Message' _) -> failure
                    Some w@(Warning _) -> lp === w
                    Some (Warning' _) -> failure
                    Some a@(IOAction _) -> lp === a
                    Some (Error _) -> failure
                    Some (FilterLog _) -> failure
                    Some (StartRun _ _) -> failure
                    Some s@(StartGroup _) -> lp === s
                    Some (StartTest _) -> failure
                    Some (StartIteration _ _) -> failure
                    Some (EndIteration _ _ _) -> failure
                    Some (EndRun _) -> failure
  in 
    lpRoundTrip genLPString checker

hprop_log_protocol_detailInfo_round_trip  :: Property
hprop_log_protocol_detailInfo_round_trip  =
  let 
    checker :: LogProtocol DetailedInfo -> Some LogProtocol -> PropertyT IO ()
    checker lp slp = 
                case slp of
                    Some (Message _) -> failure
                    Some m@(Message' _) -> lp === m
                    Some (Warning _) -> failure
                    Some m@(Warning' _) -> lp === m
                    Some (IOAction _) -> failure
                    Some (Error _) -> failure
                    Some (FilterLog _) -> failure
                    Some (StartRun _ _) -> failure
                    Some (StartGroup _) -> failure
                    Some (StartTest _) -> failure
                    Some (StartIteration _ _) -> failure
                    Some (EndIteration _ _ _) -> failure
                    Some (EndRun _) -> failure
  in 
    lpRoundTrip genLPDetailedInfo checker

hprop_log_protocol_errorInfo_round_trip  :: Property
hprop_log_protocol_errorInfo_round_trip  =
  let 
    checker :: LogProtocol AppError -> Some LogProtocol -> PropertyT IO ()
    checker lp slp = 
                case slp of
                    Some (Message _) -> failure
                    Some m@(Message' _) -> failure
                    Some (Warning _) -> failure
                    Some m@(Warning' _) -> failure
                    Some (IOAction _) -> failure
                    Some e@(Error _) -> lp === e
                    Some (FilterLog _) -> failure
                    Some (StartRun _ _) -> failure
                    Some (StartGroup _) -> failure
                    Some (StartTest _) -> failure
                    Some (StartIteration _ _) -> failure
                    Some (EndIteration _ _ _) -> failure
                    Some (EndRun _) -> failure
  in 
    lpRoundTrip genLPError checker


checkLogProtocoltc :: LogProtocol TestConfig -> Some LogProtocol -> PropertyT IO ()
checkLogProtocoltc lp slp = 
            case slp of
                Some (Message _) -> failure
                Some (Message' _) -> failure
                Some (Warning _) -> failure
                Some (Warning' _) -> failure
                Some (IOAction _) -> failure
                Some (Error _) -> failure
                Some i@(FilterLog _) -> show lp === show i 
                Some (StartRun _ _) -> failure
                Some (StartGroup _) -> failure
                Some (i@(StartTest displayInfo)) -> show lp === show i 
                Some (StartIteration _ _) -> failure
                Some (EndIteration _ _ _) -> failure
                Some (EndRun _) -> failure

hprop_log_protocol_tc_round_trip  :: Property
hprop_log_protocol_tc_round_trip = lpRoundTrip genLPTestConfig checkLogProtocoltc

failCaseData = StartTest
                    TestDisplayInfo
                      { testModAddress = TestModule "v"
                      , testTitle = "v"
                      , testConfig =
                          TestConfig
                            { header = "v"
                            , address = TestModule "v"
                            , environments = S.fromList []
                            , countries = S.fromList []
                            , minDepth = DeepRegression
                            , active = False
                            }
                      }

_failCaseData = StartTest
                    TestDisplayInfo
                      { testModAddress = TestModule "\ENQ"
                      , testTitle = "\ENQ"
                      , testConfig =
                          TestConfig
                            { header = "\ENQ"
                            , address = TestModule "\ENQ"
                            , environments = S.fromList []
                            , countries = S.fromList []
                            , minDepth = DeepRegression
                            , active = False
                            }
                      }


hprop_fail_log_protocol_tc_round_trip :: Property
hprop_fail_log_protocol_tc_round_trip  = lpRoundTrip (pure failCaseData) checkLogProtocoltc
