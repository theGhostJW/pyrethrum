module LogProtocolTest where

import           Foundation.Extended as F
import           Test.Extended as T
import qualified Prelude             as P
import DSL.LogProtocol
import Data.Functor.Identity
import Data.Functor
import DemoProject.Config
import RunElementClasses
import Data.Set as S
import Common
import TestFilter
import RunnerBase
import Data.Aeson hiding (Error)
import Data.ByteString.Lazy as B
import Hedgehog.Internal.Property
import Control.Monad.IO.Class
import Data.Traversable
import Data.Char
import Data.Aeson.TH

genStr :: Gen String
genStr = string (linear 0 1000) ascii

genTestModule :: Gen TestModule
genTestModule = TestModule <$> genStr

genTestConfig :: Gen TestConfig
genTestConfig =
  let
    set' :: (Enum a, Ord a) => Gen (Set a)
    set' = (S.fromList <$>) <$> subsequence $ enumList 
  in
    TestConfig 
      <$> genStr 
      <*> genTestModule
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

genTestDisplayInfo:: Gen TestDisplayInfo
genTestDisplayInfo = TestDisplayInfo 
                                <$> genTestModule
                                <*> genStr 
                                <*> (toJSON <$> genTestConfig)

genFilterResult:: Gen FilterResult
genFilterResult = FilterResult 
                            <$> genTestDisplayInfo
                            <*> T.maybe genStr

genFilterResults :: Gen [FilterResult]
genFilterResults =
         list (linear 0 20) genFilterResult

genInt :: Gen Int
genInt = integral $ T.linear 0 1000

genLogProtocol :: Gen LogProtocol
genLogProtocol = choice [
                    Message <$> genStr,
                    Message' <$> genDetailedInfo,
                  
                    Warning <$> genStr,
                    Warning' <$> genDetailedInfo,
                  
                    IOAction <$> genStr,
                  
                    Error . AppUserError <$> genStr,
                    FilterLog <$> genFilterResults,
                  
                    StartRun <$> genStr <*> (toJSON <$> genRunConfig),  -- title / runconfig
                    StartGroup <$> genStr,
                    StartTest <$> genTestDisplayInfo,
                    StartIteration <$> genTestModule <*> genInt <*> (toJSON <$> genRunConfig),-- iid / test module / item - using runconfig for rand om JSON object
                    Result <$> genTestModule <*> genInt <*> genStr, -- test module / iid / test Info
                    EndIteration <$> genTestModule <*> genInt, -- test module / iid / test Info
                    pure EndRun
                 ]

hprop_log_protocol_round_trip :: Property
hprop_log_protocol_round_trip = property $ do
  lp <- forAll genLogProtocol
  let 
    serialised :: B.ByteString
    serialised = encode 
                 lp

    unserialised :: Either P.String LogProtocol
    unserialised = eitherDecode serialised

  eitherf unserialised
    (\s -> footnote s *> failure)
    (lp ===)
