
module Check where

import           Foundation.Extended hiding ((.))
import           Foundation.List.DList
import Data.Function
import qualified Prelude as P
import Data.Yaml

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
                    GateFail |
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
  show = toS . (header :: Check v  -> String)

instance ToJSON (Check v)  where
  toJSON = String . toS . (header :: Check v  -> String)

instance ToJSON (CheckList a) where 
  toJSON cl = Array . fromList $ toJSON <$> toList cl

applyCheck :: forall v. v -> Check v -> CheckResult
applyCheck v ck = CheckResult (rule ck v) $ Info (header (ck :: Check v)) (msgFunc ck v)

isGateFail :: CheckOutcome -> Bool
isGateFail = \case
               GateFail -> True
               _ -> False

forceSkipped :: v -> Check v -> CheckResult
forceSkipped v ck = applyCheck v $ ck {rule = const Skip}

calcChecks :: forall v. v -> DList (Check v) -> DList CheckResult
calcChecks ds chkLst = let
                        iResult :: (Bool, a) -> Check v ->  CheckResult
                        iResult (excpt, _) = (excpt ? forceSkipped $ applyCheck) ds

                        foldfunc :: (Bool, DList CheckResult) -> Check v -> (Bool, DList CheckResult)
                        foldfunc tpl@(hasEx, lstCr) ck = let
                                                            thisChkR = iResult tpl ck
                                                          in
                                                            (hasEx || isGateFail (outcome thisChkR), cons thisChkR lstCr)
                        in
                         reverse $ snd $ foldl' foldfunc (False, mempty) chkLst

gate :: Functor f => f (Check v) -> f (Check v)
gate =  let
    gateOutcome :: Check v -> Check v
    gateOutcome ck  =
      let
        escOutcome :: CheckOutcome -> CheckOutcome
        escOutcome = \case
                         Fail -> GateFail
                         othOutcome -> othOutcome
       in
         ck {rule = escOutcome . rule ck}
  in
    (gateOutcome <$>)

-- generate a check from a predicate
chk :: String -> (v -> Bool) -> DList (Check v)
chk hdr prd = pure $ prdCheck prd hdr $ const Nothing

-- generate a check from a predicate
chk' :: String -> (v -> String) -> (v -> Bool) -> DList (Check v)
chk' hdr fMsg prd = pure $ prdCheck prd hdr $ \v -> Just $ MessageInfo hdr $ Just $ fMsg v

prdCheck :: Truthy b =>  (v -> b) -> String -> (v -> Maybe MessageInfo) -> Check v
prdCheck prd hdr = Check hdr (\v -> prd v ? Pass $ Fail)
