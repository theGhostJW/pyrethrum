
module Check where

import           Foundation.Extended hiding ((.))
import           Foundation.List.DList
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

data CheckOutcome = Pass |
                    Fail |
                    Exception |
                    Skip
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
forceSkipped v (Check rule info) = applyCheck v (Check (const Skip) info)

calcChecks :: forall v. v -> DList (v -> Check v )-> DList CheckResult
calcChecks vs chks = let
                      chkLst = (vs &) <$> chks

                      iResult :: forall a. (Bool, a) -> Check v ->  CheckResult
                      iResult (excpt, _) = (excpt ? forceSkipped $ applyCheck) vs

                      foldfunc :: (Bool, DList CheckResult) -> Check v -> (Bool, DList CheckResult)
                      foldfunc tpl@(hasEx, lstCr) ck = let
                                                          thisChkR = iResult tpl ck
                                                        in
                                                          (hasEx || isExcption (outcome thisChkR), cons thisChkR lstCr)
                      in
                       reverse $ snd $ foldl' foldfunc (False, mempty) chkLst

escalate :: Check v -> Check v
escalate (Check rulef infof) =
  let
    escOutcome o = case o of
                     Fail -> Exception
                     _ -> o
   in
    Check (escOutcome . rulef) infof

chk :: String -> (v -> Bool) -> DList (v -> Check v)
chk msg prd = pure $ chkSingular msg prd

chk' :: String -> (v -> Bool) -> DList (v -> Check v)
chk' msg prd = (escalate <$>) <$> chk msg prd

chkSingular :: String -> (v -> Bool) -> v -> Check v
chkSingular msg prd val = let
                            rsltType = prd val ? Pass $ Fail
                          in
                            Check (const rsltType) $ const $ Info msg Nothing
