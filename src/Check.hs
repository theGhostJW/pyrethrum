
module Check where

import           Foundation.Extended
import Data.Function

data MessageInfo = MessageInfo {
                                  message :: String,
                                  additionalInfo :: Maybe String
                                }
                                deriving (Show, Eq)

data CheckInfo = Info {
                     header :: String,
                     messageInfo :: Maybe MessageInfo
                   }
                   deriving (Show, Eq)

data CheckOutcome = Success |
                    Failure |
                    Exception |
                    Skipped
                    deriving (Show, Eq)

data CheckResult = CheckResult {
    outcome :: CheckOutcome,
    info :: CheckInfo
  }
  deriving (Show, Eq)

data Check v = Check {
    rule :: v -> CheckOutcome,
    infoFunc :: v -> CheckInfo
  }

applyCheck :: v -> Check v -> CheckResult
applyCheck v ck = CheckResult (rule ck v) (infoFunc ck v)

isExcption :: CheckOutcome -> Bool
isExcption = \case
               Exception -> True
               _ -> False

forceSkipped :: v -> Check v -> CheckResult
forceSkipped v (Check rule info) = applyCheck v (Check (const Skipped) info)

calcChecks :: forall v. v -> [v -> Check v] -> [CheckResult]
calcChecks vs chks = let
                      chkLst = (vs &) <$> chks

                      iResult :: forall a. (Bool, a) -> Check v ->  CheckResult
                      iResult (excpt, _) = (excpt ? forceSkipped $ applyCheck) vs

                      foldfunc :: (Bool, [CheckResult]) -> Check v ->  (Bool, [CheckResult])
                      foldfunc tpl@(hasEx, lstCr) ck = let
                                                          thisChkR = iResult tpl ck
                                                        in
                                                          (hasEx || isExcption (outcome thisChkR), thisChkR : lstCr)
                      in
                       snd $ foldl' foldfunc (False, []) chkLst


chk :: String -> (v -> Bool) -> (v -> Check v)
chk msg prd val = let
                    rsltType = prd val ? Success $ Failure
                  in
                    Check (const rsltType) $ const $ Info msg Nothing
