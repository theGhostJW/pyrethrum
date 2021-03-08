module Check (
              calcChecks,
              classifyResult,
              chk,
              chk',
              expectFailure,
              expectFailureFixed,
              gate,
              gateAll,
              skipChecks,
              ExpectationActive(..),
              ResultExpectation(..),
              GateStatus(..),
              Check(..),
              CheckDList,
              CheckReportList,
              CheckReport(..),
              CheckResult(..),
              MessageInfo(..),
              CheckResultClassification(..)
              ) where

import           Prelude as P hiding ((.))
import           Data.Text
import           Data.DList as D
import Data.Function
import Data.Aeson.Types as AT hiding (Error) 
import Data.Aeson.TH
import qualified Data.List as L

-- generate a check from a predicate
chk :: Text -> (v -> Bool) -> DList (Check v)
chk hdr prd = pure $ prdCheck prd hdr $ const Nothing

-- generate a check from a predicate with detailed message
chk' :: Text -> (v -> Text) -> (v -> Bool) -> DList (Check v)
chk' hdr fMsg prd = pure $ prdCheck prd hdr $ \v -> Just $ fMsg v


data Check ds = Check {
    header :: Text,
    rule :: ds -> Bool,
    msgFunc :: ds -> Maybe Text,
    expectation :: ResultExpectation,
    gateStatus :: GateStatus
  }

data SpecialCheck = SpecialCheck {
    rule :: Text,
    expectation :: ResultExpectation,
    isGate :: Bool
  } deriving Show 

toDisplay :: Check v -> SpecialCheck
toDisplay Check{..} = SpecialCheck header expectation (GateCheck == gateStatus)

prdCheck :: forall ds. (ds -> Bool) -> Text -> (ds -> Maybe Text) -> Check ds
prdCheck prd hdr msgf = Check {
                          header = hdr,
                          rule = prd,
                          msgFunc = msgf,
                          expectation = ExpectPass,
                          gateStatus = StandardCheck
                        }

applyToFirst :: (Check ds -> Check ds) -> DList (Check ds) -> DList (Check ds)
applyToFirst f = \case
                    Nil -> D.empty
                    Cons x xs -> D.cons (f x) $ D.fromList xs
                    _ -> error "DList case failure - should not happen"

gate :: forall ds. DList (Check ds) -> DList (Check ds)
gate = applyToFirst (\c -> (c :: Check ds){gateStatus = GateCheck})

gateAll :: forall ds. DList (Check ds) -> DList (Check ds)
gateAll fck = (\ck -> (ck :: Check ds) {gateStatus = GateCheck}) <$> fck

expectFailurePriv :: forall ds. ExpectationActive -> Text -> DList (Check ds) -> DList (Check ds)
expectFailurePriv isActive msg = applyToFirst (\c -> (c :: Check ds){expectation = ExpectFailure isActive msg})

expectFailure :: Text -> DList (Check ds) -> DList (Check ds)
expectFailure = expectFailurePriv Active

expectFailureFixed :: Text -> DList (Check ds) -> DList (Check ds)
expectFailureFixed = expectFailurePriv Inactive

type CheckReportList = DList CheckReport
type CheckDList a = DList (Check a)

data MessageInfo = MessageInfo {
                                  message :: Text,
                                  additionalInfo :: Maybe Text
                                }
                                deriving (Show, Eq)

data CheckResult = Pass |
                   Fail |
                   GateFail |
                   FailExpected Text |
                   GateFailExpected Text |
                   PassWhenFailExpected Text|
                   Regression Text |
                   GateRegression Text |
                   Skip
                    deriving (Show, Eq)

-- order by severity
instance Ord CheckResult where 
  v0 <= v1 = 
    let 
      idx :: CheckResult -> Int
      idx = \case 
              Pass -> 0
              Skip -> 1
              FailExpected _ -> 2
              GateFailExpected _ -> 3
              PassWhenFailExpected _ -> 4
              Fail -> 5
              GateFail -> 6
              Regression _ -> 7
              GateRegression _ -> 8
    in 
      idx v0 <= idx v1

data CheckResultClassification = 
                   OK |
                   Error |
                   Warning |
                   Skipped
                    deriving (Show, Eq)

classifyResult :: CheckResult -> CheckResultClassification
classifyResult  = \case 
                      Pass -> OK
                      Fail -> Error
                      GateFail -> Error
                      FailExpected _ -> Warning
                      GateFailExpected _ -> Warning
                      PassWhenFailExpected _ -> Error
                      Regression _ -> Error
                      GateRegression _ -> Error
                      Skip -> Skipped

data ExpectationActive = Active | Inactive deriving (Show, Eq)

data ResultExpectation = ExpectPass 
                          | ExpectFailure {
                                            isActive :: ExpectationActive,
                                            reason :: Text
                                          }
                          deriving (Show, Eq)

data GateStatus = GateCheck 
              | StandardCheck
              deriving (Show, Eq)

data CheckReport = CheckReport {
    result :: CheckResult,
    info :: MessageInfo
  }
  deriving (Show, Eq)

instance P.Show (Check v) where
  show ck@Check{..} = unpack $ 
                        if  gateStatus == GateCheck && (expectation == ExpectPass)
                        then  header 
                        else  pack . show $ toDisplay ck

instance ToJSON (Check v)  where
  toJSON = undefined


reverseDList :: DList a -> DList a
reverseDList = D.fromList . P.reverse . D.toList

isGateFail :: CheckResult -> Bool
isGateFail = \case 
                Pass -> False
                Fail -> False
                GateFail -> True
                FailExpected _ -> False
                GateFailExpected _ -> True
                PassWhenFailExpected _ -> False
                Regression _ -> False
                GateRegression _ -> True
                Skip -> False

skipChecks :: DList (Check ds) -> DList CheckReport
skipChecks chks = 
  let 
    skippedResult :: Check ds -> CheckReport
    skippedResult (Check headr _ _ _ _)  = CheckReport Skip $ MessageInfo headr $ Just "Validation checks not executed"
  in 
    reverseDList $ skippedResult <$> chks 

calcChecks :: forall ds. ds -> DList (Check ds) -> DList CheckReport
calcChecks ds chkLst = undefined 

$(deriveJSON defaultOptions ''MessageInfo)
$(deriveJSON defaultOptions ''CheckResult)
$(deriveJSON defaultOptions ''CheckReport)
$(deriveJSON defaultOptions ''ResultExpectation)
$(deriveJSON defaultOptions ''ExpectationActive)
$(deriveJSON defaultOptions ''GateStatus)
                         