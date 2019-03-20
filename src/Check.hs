module Check (
              calcChecks,
              classifyResult,
              chk,
              chk',
              expectFailure,
              expectFailureFixed,
              gate,
              gateAll,
              ExpectationActive(..),
              ResultExpectation(..),
              GateStatus(..),
              Check(..),
              CheckDList,
              CheckReportList,
              CheckReport(..),
              CheckResult(..),
              CheckInfo(..)
              ) where

import           Pyrelude hiding ((.))
import           Data.DList as D
import Data.Function
import qualified Prelude as P 
import Data.Aeson.Types hiding (Error)
import Data.Aeson.TH
import OrphanedInstances
import Common

-- generate a check from a predicate
chk :: Text -> (v -> Bool) -> DList (Check v)
chk hdr prd = pure $ prdCheck prd hdr $ const Nothing

-- generate a check from a predicate with detailed message
chk' :: Text -> (v -> Text) -> (v -> Bool) -> DList (Check v)
chk' hdr fMsg prd = pure $ prdCheck prd hdr $ \v -> Just $ MessageInfo hdr $ Just $ fMsg v


data Check ds = Check {
    header :: Text,
    rule :: ds -> Bool,
    msgFunc :: ds -> Maybe MessageInfo,
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

prdCheck :: forall ds. (ds -> Bool) -> Text -> (ds -> Maybe MessageInfo) -> Check ds
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
                    Cons x xs -> D.cons (f x) xs

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

data CheckInfo = CheckInfo {
                     header :: Text,
                     messageInfo :: Maybe MessageInfo
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
    info :: CheckInfo
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''MessageInfo)
$(deriveJSON defaultOptions ''CheckInfo)
$(deriveJSON defaultOptions ''CheckResult)
$(deriveJSON defaultOptions ''CheckReport)
$(deriveJSON defaultOptions ''ResultExpectation)
$(deriveJSON defaultOptions ''ExpectationActive)
$(deriveJSON defaultOptions ''GateStatus)

instance P.Show (Check v) where
  show ck@Check{..} = toS $ 
                        gateStatus == GateCheck && (expectation == ExpectPass)?
                          header $
                          show $ toDisplay ck

instance ToJSON (Check v)  where
  toJSON = Text . toS . (header :: Check v  -> Text)

instance ToJSON (CheckDList a) where 
  toJSON cl = Array . fromList $ toJSON <$> cl

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

calcChecks :: forall ds. ds -> DList (Check ds) -> DList CheckReport
calcChecks ds chkLst = let
                        applyCheck :: Bool -> Check ds -> CheckReport
                        applyCheck skip Check{..} = 
                          let 
                            isGate :: Bool
                            isGate = gateStatus == GateCheck

                            rslt :: CheckResult
                            rslt = skip ? 
                                      Skip $     -- skip cases
                                      rule ds ?  -- pass case 
                                                ( 
                                                  case expectation of 
                                                    ExpectPass -> Pass 
                                                    ExpectFailure Active msg-> PassWhenFailExpected msg 
                                                    ExpectFailure Inactive _ -> Pass
                                                ) 
                                              $  -- fail cases
                                                ( 
                                                  case expectation of 
                                                    ExpectPass -> isGate ? GateFail $ Fail   
                                                    ExpectFailure Active msg -> (isGate ? GateFailExpected $ FailExpected) msg
                                                    ExpectFailure Inactive msg -> (isGate ? GateRegression $ Regression) msg
                                                )
                          in 
                            CheckReport rslt $ CheckInfo header (msgFunc ds)

                        foldfunc :: (Bool, DList CheckReport) -> Check ds -> (Bool, DList CheckReport)
                        foldfunc (wantSkip, lstCr) ck = let
                                                          thisChkR :: CheckReport
                                                          thisChkR = applyCheck wantSkip ck
                                                        in
                                                          (wantSkip || isGateFail (result thisChkR), D.cons thisChkR lstCr)
                        in
                         reverse $ snd $ foldl' foldfunc (False, mempty) chkLst
