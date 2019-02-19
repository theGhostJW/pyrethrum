
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
                    GuardFail |
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

instance ToJSON (Check v)  where
  toJSON = String . toText . (header :: Check v  -> String)

instance ToJSON (CheckList a) where 
  toJSON cl = Array . fromList $ toJSON <$> toList cl

applyCheck :: forall v. v -> Check v -> CheckResult
applyCheck v ck = CheckResult (rule ck v) $ Info (header (ck :: Check v)) (msgFunc ck v)

isGuardFail :: CheckOutcome -> Bool
isGuardFail = \case
               GuardFail -> True
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
                                                            (hasEx || isGuardFail (outcome thisChkR), cons thisChkR lstCr)
                        in
                         reverse $ snd $ foldl' foldfunc (False, mempty) chkLst

-- TODO: clashes with guard on Alternative rename ??
guard :: Functor f => f (Check v) -> f (Check v)
guard =  let
    guardOutcome :: Check v -> Check v
    guardOutcome ck  =
      let
        escOutcome o = case o of
                         Fail -> GuardFail
                         _ -> o
       in
         ck {rule = escOutcome . rule ck}
  in
    (guardOutcome <$>)

-- generate a check from a predicate
chk :: String -> (v -> Bool) -> DList (Check v)
chk hdr prd = pure $ prdCheck prd hdr $ const Nothing

-- generate a check from a predicate
chk' :: String -> (v -> String) -> (v -> Bool) -> DList (Check v)
chk' hdr fMsg prd = pure $ prdCheck prd hdr $ \v -> Just $ MessageInfo hdr $ Just $ fMsg v

prdCheck :: Truthy b =>  (v -> b) -> String -> (v -> Maybe MessageInfo) -> Check v
prdCheck prd hdr = Check hdr (\v -> prd v ? Pass $ Fail)
