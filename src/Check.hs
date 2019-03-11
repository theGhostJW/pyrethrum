module Check (
              calcChecks,
              classifyResult,
              chk,
              chk',
              expectFailure,
              expectFailureFixed,
              gate,
              gateFirst,
              ExpectationActive(..),
              ResultExpectation(..),
              GateStatus(..),
              Check(..),
              CheckList,
              CheckReportList,
              CheckReport(..),
              CheckResult(..),
              CheckInfo(..)
              ) where

import           Foundation.Extended hiding ((.))
import           Foundation.List.DList
import Data.Function
import qualified Prelude as P 
import Data.Aeson.Types hiding (Error)
import Data.Aeson.TH
import OrphanedInstances

-- generate a check from a predicate
chk :: String -> (v -> Bool) -> DList (Check v)
chk hdr prd = pure $ prdCheck prd hdr $ const Nothing

-- generate a check from a predicate with detailed message
chk' :: String -> (v -> String) -> (v -> Bool) -> DList (Check v)
chk' hdr fMsg prd = pure $ prdCheck prd hdr $ \v -> Just $ MessageInfo hdr $ Just $ fMsg v


data Check v = Check {
    header :: String,
    rule :: v -> Bool,
    msgFunc :: v -> Maybe MessageInfo,
    expectation :: ResultExpectation,
    gateStatus :: GateStatus
  }

prdCheck :: forall ds. (ds -> Bool) -> String -> (ds -> Maybe MessageInfo) -> Check ds
prdCheck prd hdr msgf = Check {
                          header = hdr,
                          rule = prd,
                          msgFunc = msgf,
                          expectation = ExpectPass,
                          gateStatus = StandardCheck
                        }

gateFirst :: DList (Check ds) -> DList (Check ds)
gateFirst chks = fromList $ case toList chks of 
                        [] -> []
                        x:xs -> x {gateStatus = GateCheck} : xs

gate :: forall f v. Functor f => f (Check v) -> f (Check v)
gate fck = (\ck -> ck {gateStatus = GateCheck}) <$> fck

expectFailurePriv :: forall f v. Functor f => ExpectationActive -> String -> f (Check v) -> f (Check v)
expectFailurePriv isActive msg fck = (\ck -> ck {expectation = ExpectFailure msg isActive}) <$> fck

expectFailure :: forall f v. Functor f => String -> f (Check v) -> f (Check v)
expectFailure = expectFailurePriv Active

expectFailureFixed :: forall f v. Functor f => String -> f (Check v) -> f (Check v)
expectFailureFixed = expectFailurePriv Inactive

type CheckReportList = DList CheckReport
type CheckList a = DList (Check a)

data MessageInfo = MessageInfo {
                                  message :: String,
                                  additionalInfo :: Maybe String
                                }
                                deriving (Show, Eq)

data CheckInfo = CheckInfo {
                     header :: String,
                     messageInfo :: Maybe MessageInfo
                   }
                   deriving (Show, Eq)

data CheckResult = Pass |
                   Fail |
                   GateFail |
                   FailExpected String |
                   GateFailExpected String |
                   PassWhenFailExpected String|
                   Regression String |
                   GateRegression String |
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
                          | ExpectFailure String ExpectationActive
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
  show = toS . (header :: Check v  -> String)

instance ToJSON (Check v)  where
  toJSON = String . toS . (header :: Check v  -> String)

instance ToJSON (CheckList a) where 
  toJSON cl = Array . fromList $ toJSON <$> toList cl


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
                                                    ExpectFailure msg Active -> PassWhenFailExpected msg 
                                                    ExpectFailure _ Inactive -> Pass
                                                ) 
                                              $  -- fail cases
                                                ( 
                                                  case expectation of 
                                                    ExpectPass -> isGate ? GateFail $ Fail   
                                                    ExpectFailure msg Active -> (isGate ? GateFailExpected $ FailExpected) msg
                                                    ExpectFailure msg Inactive -> (isGate ? GateRegression $ Regression) msg
                                                )
                          in 
                            CheckReport rslt $ CheckInfo header (msgFunc ds)

                        foldfunc :: (Bool, DList CheckReport) -> Check ds -> (Bool, DList CheckReport)
                        foldfunc (wantSkip, lstCr) ck = let
                                                          thisChkR :: CheckReport
                                                          thisChkR = applyCheck wantSkip ck
                                                        in
                                                          (wantSkip || isGateFail (result thisChkR), cons thisChkR lstCr)
                        in
                         reverse $ snd $ foldl' foldfunc (False, mempty) chkLst
