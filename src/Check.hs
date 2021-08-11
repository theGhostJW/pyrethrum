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
              Checks,
              CheckReportList,
              CheckReport(..),
              CheckResult(..),
              CheckResultClassification(..)
              ) where

import           Pyrelude as P hiding ((.)) 
import           Data.DList as D
import Data.Function
import Data.Aeson.Types as AT hiding (Error) 
import Data.Aeson.TH
import qualified Data.List as L
import Common (DetailedInfo(DetailedInfo))

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
type Checks a = DList (Check a)

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
    info :: DetailedInfo
  }
  deriving (Show, Eq)

instance P.Show (Check v) where
  show ck@Check{..} = toS $ 
                        gateStatus == GateCheck && (expectation == ExpectPass)?
                          header $
                          txt $ toDisplay ck

instance ToJSON (Check v)  where
  toJSON = String . toS . (header :: Check v  -> Text)


reverseDList :: DList a -> DList a
reverseDList = D.fromList . reverse . D.toList

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
    skippedResult (Check headr _ _ _ _)  = CheckReport Skip $ DetailedInfo headr "Validation checks not executed"
  in 
    reverseDList $ skippedResult <$> chks 

calcChecks :: forall ds. ds -> DList (Check ds) -> DList CheckReport
calcChecks ds chkLst = 
  let
    applyCheck :: Bool -> Check ds -> CheckReport
    applyCheck skip (Check header rule msgFunc expectation gateStatus) = 
      let 
        isGate :: Bool
        isGate = gateStatus == GateCheck

        rslt :: CheckResult
        rslt = skip
                 ? Skip    -- skip cases
                   $ rule ds 
                       ? ( -- pass case 
                            case expectation of 
                              ExpectPass -> Pass 
                              ExpectFailure Active msg-> PassWhenFailExpected msg 
                              ExpectFailure Inactive _ -> Pass
                        ) 
                        $ ( -- fail cases
                            case expectation of 
                              ExpectPass -> isGate ? GateFail $ Fail   
                              ExpectFailure Active msg -> (isGate ? GateFailExpected $ FailExpected) msg
                              ExpectFailure Inactive msg -> (isGate ? GateRegression $ Regression) msg
                        )
      in 
        CheckReport rslt $ DetailedInfo header (fromMaybe "" $ msgFunc ds)

    foldfunc :: (Bool, DList CheckReport) -> Check ds -> (Bool, DList CheckReport)
    foldfunc (wantSkip, lstCr) ck = let
                                      thisChkR :: CheckReport
                                      thisChkR = applyCheck wantSkip ck
                                    in
                                      (wantSkip || isGateFail (result thisChkR), D.cons thisChkR lstCr)
    in
      reverseDList . snd $ L.foldl' foldfunc (False, mempty) chkLst

$(deriveJSON defaultOptions ''CheckResult)
$(deriveJSON defaultOptions ''CheckReport)
$(deriveJSON defaultOptions ''ResultExpectation)
$(deriveJSON defaultOptions ''ExpectationActive)
$(deriveJSON defaultOptions ''GateStatus)
                         