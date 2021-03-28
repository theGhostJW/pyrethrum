module LogProtocolTest where

import           Pyrelude as P
import           Pyrelude.Test as T
import DSL.LogProtocol as LP
import RunElementClasses
import Data.Set as S
import Common
import qualified Data.Aeson as A
import Data.ByteString.Lazy as B
import qualified Check as C
import Data.Aeson
import Data.Aeson.TH

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum)
data Country = AU | NZ deriving (Show, Eq, Ord, Enum)
data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)

data TestConfig = TestConfig {
  header       :: Text,
  address      :: TestAddress,
  environments :: Set Environment,
  countries    :: Set Country,
  minDepth     :: Depth,
  active       :: Bool
}  deriving (Eq, Show)

data RunConfig = RunConfig {
  runTitle    :: Text,
  environment :: Environment,
  country     :: Country,
  depth       :: Depth
} deriving (Eq, Show)

runConfig :: RunConfig
runConfig = RunConfig {
  runTitle = "Sample RunConfig",
  environment = TST,
  country = AU,
  depth = DeepRegression
}

genJSON :: Gen A.Value
genJSON = A.toJSON <$> genRunConfig -- using runconfig as easy proxy for random aeson

genStr :: Gen Text
genStr = text (linear 0 1000) ascii

genTestAddress :: Gen TestAddress
genTestAddress = TestAddress <$> genStr

genTestConfig :: Gen TestConfig
genTestConfig =
  let
    set' :: (Enum a, Ord a) => Gen (Set a)
    set' = (S.fromList <$>) <$> subsequence $ enumList 
  in
    TestConfig 
      <$> genStr 
      <*> genTestAddress
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
                                <$> genTestAddress
                                <*> genStr 
                                <*> (A.toJSON <$> genTestConfig)

genTestFilterResult:: Gen TestFilterResult
genTestFilterResult = TestFilterResult 
                            <$> genTestDisplayInfo
                            <*> T.maybe genStr

genTestFilterResults :: Gen [TestFilterResult]
genTestFilterResults =
         T.list (linear 0 20) genTestFilterResult

genInt :: Gen Int
genInt = integral $ T.linear 0 1000

genItemId :: Gen ItemId
genItemId  = ItemId <$> genTestAddress <*> genInt

genError :: Gen (FrameworkError Int)
genError = Common.Error <$> genStr

genDocActionInfo :: Gen DocActionInfo
genDocActionInfo = choice [
  ActionInfo <$> genStr,
  ActionInfo' <$> genStr <*> genStr
 ]

genResultExpectation :: Gen C.ResultExpectation
genResultExpectation = choice [
  pure C.ExpectPass, 
  C.ExpectFailure C.Inactive <$> genStr,
  C.ExpectFailure C.Active <$> genStr 
 ]

genGateStatus :: Gen C.GateStatus
genGateStatus = choice [
  pure C.StandardCheck,
  pure C.GateCheck
 ]

genLogProtocol :: Gen (LogProtocolBase Int)
genLogProtocol = choice [
                    BoundaryLog <$> (StartRun <$> (RunTitle <$> genStr) <*> genInt <*> (A.toJSON <$> genRunConfig)), 
                    BoundaryLog . StartGroup <$> (GroupTitle <$> genStr),
                    BoundaryLog . EndGroup <$> (GroupTitle <$> genStr),
                    BoundaryLog . StartTest <$> genTestDisplayInfo,
                    BoundaryLog . EndTest <$> genTestAddress,
                    BoundaryLog <$> (StartIteration <$> genItemId <*> (WhenClause <$> genStr) <*> (ThenClause <$> genStr) <*> (A.toJSON <$> genRunConfig)), --- using runconfig as an easy proxy for item
                    BoundaryLog . EndIteration <$> genItemId,
                    BoundaryLog . FilterLog <$> genTestFilterResults,
                    pure $ BoundaryLog EndRun,

                    logDoc . DocAction <$> genDocActionInfo,
                    logDoc . DocIOAction <$> genStr,
                    logDoc <$> (DocCheck <$> genItemId <*> genStr <*>  genResultExpectation <*> genGateStatus),
                    logDoc . DocMessage <$> genStr,
                    logDoc . DocMessage' <$> genDetailedInfo,
                  
                    logDoc . DocWarning <$> genStr,
                    logDoc . DocWarning' <$> genDetailedInfo,
                    logDoc . DocError <$> genError,

                    logRun . IOAction <$> genStr,
                    logRun <$> (InteractorSuccess <$> genItemId <*> (ApStateJSON <$> genJSON)),
                    logRun <$> (InteractorFailure <$> genItemId <*> genError),

                    logRun <$> (ParserSuccess <$> genItemId <*> (DStateJSON <$> genJSON)),
                    logRun <$> (ParserFailure <$> genItemId <*> genError),

                    logRun . Message <$> genStr,
                    logRun . Message' <$> genDetailedInfo,
                  
                    logRun . Warning <$> genStr,
                    logRun . Warning' <$> genDetailedInfo,
                    logRun . LP.Error <$> genError
                 ]



hprop_log_protocol_round_trip :: Property
hprop_log_protocol_round_trip = property $ do
  lp <- forAll genLogProtocol
  let 
    serialised :: B.ByteString
    serialised = A.encode lp

    unserialised :: Either Text (LogProtocolBase Int)
    unserialised = mapLeft txt $ A.eitherDecode serialised

  eitherf unserialised
    (\s -> footnote (toS s) *> failure)
    (lp ===)

$(deriveJSON defaultOptions ''TestConfig)
$(deriveJSON defaultOptions ''Environment)
$(deriveJSON defaultOptions ''Country)
$(deriveJSON defaultOptions ''Depth)
$(deriveJSON defaultOptions ''RunConfig)
