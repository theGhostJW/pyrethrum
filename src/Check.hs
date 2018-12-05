
module Check where

import           Foundation.Extended hiding ((.))
import           Foundation.List.DList
import Data.Function
import qualified Prelude as P

type CheckResultList = DList CheckResult
type CheckList a = DList (Check a)

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
    header :: String,
    rule :: v -> CheckOutcome,
    msgFunc :: v -> Maybe MessageInfo
  }

instance P.Show (Check v) where
  show = fromStr . (header :: Check v  -> String)

applyCheck :: forall v. v -> Check v -> CheckResult
applyCheck v ck = CheckResult (rule ck v) $ Info (header (ck :: Check v)) (msgFunc ck v)

isExcption :: CheckOutcome -> Bool
isExcption = \case
               Exception -> True
               _ -> False

forceSkipped :: v -> Check v -> CheckResult
forceSkipped v ck = applyCheck v $ ck {rule = const Skip}


calcChecks :: forall v. v -> DList (Check v) -> DList CheckResult
calcChecks vs chkLst = let
                        iResult :: forall a. (Bool, a) -> Check v ->  CheckResult
                        iResult (excpt, _) = (excpt ? forceSkipped $ applyCheck) vs

                        foldfunc :: (Bool, DList CheckResult) -> Check v -> (Bool, DList CheckResult)
                        foldfunc tpl@(hasEx, lstCr) ck = let
                                                            thisChkR = iResult tpl ck
                                                          in
                                                            (hasEx || isExcption (outcome thisChkR), cons thisChkR lstCr)
                        in
                         reverse $ snd $ foldl' foldfunc (False, mempty) chkLst

guard :: Functor f => f (Check v) -> f (Check v)
guard =  let
    guardOutcome :: Check v -> Check v
    guardOutcome ck  =
      let
        escOutcome o = case o of
                         Fail -> Exception
                         _ -> o
       in
         ck {rule = escOutcome . rule ck}
  in
    (guardOutcome <$>)

-- generate a check from a predicate
prdCheck :: Truthy b =>  (v -> b) -> String -> (v -> Maybe MessageInfo) -> Check v
prdCheck prd hdr = Check hdr (\v -> prd v ? Pass $ Fail)

chkGuardPriv :: String -> (v -> Bool) -> (v -> Maybe MessageInfo) -> DList (Check v)
chkGuardPriv hdr prd msgf = pure $ prdCheck prd hdr msgf

chk :: String -> (v -> Bool) -> DList (Check v)
chk hdr prd = chkGuardPriv hdr prd $ const Nothing

chkGuard :: String -> (v -> Bool) -> (v -> String) -> DList (Check v)
chkGuard hdr prd msgf = chkGuardPriv hdr prd $ \v -> Just $ MessageInfo (msgf v) Nothing
