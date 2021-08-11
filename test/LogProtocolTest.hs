module LogProtocolTest where

import qualified Check as C
import Common
import DSL.LogProtocol as LP
import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Lazy as B
import Data.Set as S
import Pyrelude as P
import Pyrelude.Test as T
import RunElementClasses hiding (element)

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord, Enum)

data Country = AU | NZ deriving (Show, Eq, Ord, Enum)

data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord, Enum)

data TestConfig = TestConfig
  { header :: Text,
    environments :: Set Environment,
    countries :: Set Country,
    minDepth :: Depth,
    active :: Bool
  }
  deriving (Eq, Show)

data RunConfig = RunConfig
  { runTitle :: Text,
    environment :: Environment,
    country :: Country,
    depth :: Depth
  }
  deriving (Eq, Show)

runConfig :: RunConfig
runConfig =
  RunConfig
    { runTitle = "Sample RunConfig",
      environment = TST,
      country = AU,
      depth = DeepRegression
    }

genJSON :: Gen A.Value
genJSON = A.toJSON <$> genRunConfig -- using runconfig as easy proxy for random aeson

genTxt :: Gen Text
genTxt = text (linear 0 1000) ascii


modDomain :: Address
modDomain = P.foldl' (flip push) rootAddress ["root", "sub 1", "sub 2"]

domainElementSingleton :: Gen Address
domainElementSingleton = pure $ push "test" modDomain

addressSingleton :: Gen Address
addressSingleton = pure modDomain

genTestConfig :: Gen TestConfig
genTestConfig =
  let set' :: (Enum a, Ord a) => Gen (Set a)
      set' = (S.fromList <$>) <$> subsequence $ enumList
   in TestConfig
        <$> genTxt
        <*> set'
        <*> set'
        <*> T.element enumList
        <*> T.bool

genRunConfig :: Gen RunConfig
genRunConfig =
  RunConfig
    <$> genTxt
    <*> element enumList
    <*> element enumList
    <*> element enumList

genDetailedInfo :: Gen DetailedInfo
genDetailedInfo = DetailedInfo <$> genTxt <*> genTxt

genTestDisplayInfo :: Gen TestLogInfo
genTestDisplayInfo =
  TestLogInfo
    <$>  genTxt
    <*>  addressSingleton
    <*> (A.toJSON <$> genTestConfig)

genTestFilterResult :: Gen TestFilterResult
genTestFilterResult =
  TestFilterResult
    <$> genTestDisplayInfo
    <*> T.maybe genTxt

genTestFilterResults :: Gen [TestFilterResult]
genTestFilterResults =
  T.list (linear 0 20) genTestFilterResult

genInt :: Gen Int
genInt = integral $ T.linear 0 1000

genItemId :: Gen ItemId
genItemId = ItemId <$> domainElementSingleton <*> genInt

genError :: Gen (FrameworkError Int)
genError = Common.Error <$> genTxt

genResultExpectation :: Gen C.ResultExpectation
genResultExpectation =
  choice
    [ pure C.ExpectPass,
      C.ExpectFailure C.Inactive <$> genTxt,
      C.ExpectFailure C.Active <$> genTxt
    ]

genGateStatus :: Gen C.GateStatus
genGateStatus =
  choice
    [ pure C.StandardCheck,
      pure C.GateCheck
    ]

genLogProtocol :: Gen (LogProtocolBase Int)
genLogProtocol =
  choice
    [ StartRun <$> (RunTitle <$> genTxt) <*> genInt <*> (A.toJSON <$> genRunConfig),
      StartGroup <$> (GroupTitle <$> genTxt),
      EndGroup <$> (GroupTitle <$> genTxt),
      StartTest <$> genTestDisplayInfo,
      EndTest <$> domainElementSingleton,
      StartIteration <$> genItemId <*> genTxt <*> (A.toJSON <$> genRunConfig), --- using runconfig as an easy proxy for item
      EndIteration <$> genItemId,
      FilterLog <$> genTestFilterResults,
      pure EndRun,
      IOAction <$> genTxt,
      IOAction' <$> (DetailedInfo <$> genTxt <*> genTxt),
      InteractorSuccess <$> genItemId <*> (ApStateJSON <$> genJSON),
      InteractorFailure <$> genItemId <*> genError,
      ParserSuccess <$> genItemId <*> (DStateJSON <$> genJSON),
      ParserFailure <$> genItemId <*> genError,
      Message <$> genTxt,
      Message' <$> genDetailedInfo,
      Warning <$> genTxt,
      Warning' <$> genDetailedInfo,
      LP.Error <$> genError
    ]

hprop_log_protocol_round_trip :: Property
hprop_log_protocol_round_trip = property $ do
  lp <- forAll genLogProtocol
  let serialised :: B.ByteString
      serialised = A.encode lp

      unserialised :: Either Text (LogProtocolBase Int)
      unserialised = mapLeft txt $ A.eitherDecode serialised

  eitherf
    unserialised
    (\s -> footnote (toS s) *> failure)
    (lp ===)

$(deriveJSON defaultOptions ''TestConfig)
$(deriveJSON defaultOptions ''Environment)
$(deriveJSON defaultOptions ''Country)
$(deriveJSON defaultOptions ''Depth)
$(deriveJSON defaultOptions ''RunConfig)
