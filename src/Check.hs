
module Check where

import           Foundation.Extended hiding ((.))
import           Foundation.List.DList
import Data.Function
import qualified Prelude as P

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

instance P.Show (v -> Check v) where
  show = undefined

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

escalate :: (Functor f, Functor g) => f (g (Check v)) -> f (g (Check v))
escalate =
  let
    escalateOutcome :: Check v -> Check v
    escalateOutcome (Check rulef infof) =
      let
        escOutcome o = case o of
                         Fail -> Exception
                         _ -> o
       in
        Check (escOutcome . rulef) infof
  in
    ((escalateOutcome <$>) <$> )

-- generate a check from a predicate
prdCheck :: Truthy b => (v -> b) -> v -> (v -> CheckInfo) -> Check v
prdCheck prd val = Check (const $ prd val ? Pass $ Fail)

chkmPriv :: String -> (v -> Bool) -> (v -> Maybe MessageInfo) -> DList (v -> Check v)
chkmPriv hdr prd msgf = pure
                      $ \v -> prdCheck prd v $ const $ Info hdr $ msgf v

chk :: String -> (v -> Bool) -> DList (v -> Check v)
chk hdr prd = chkmPriv hdr prd $ const Nothing

chk' :: String -> (v -> Bool) -> DList (v -> Check v)
chk' msg prd = escalate $ chk msg prd

chkm :: String -> (v -> Bool) -> (v -> String) -> DList (v -> Check v)
chkm hdr prd msgf = chkmPriv hdr prd $ \v -> Just $ MessageInfo (msgf v) Nothing

chkm' :: String -> (v -> Bool) -> (v -> String) -> DList (v -> Check v)
chkm' hdr prd msgf = escalate $ chkm hdr prd msgf
