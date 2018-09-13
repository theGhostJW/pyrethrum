
module Check where

import           Foundation.Extended

data CheckMessage = Brief String |
                    Extended {
                     message        :: String,
                     additionalInfo :: String
                   }
                   deriving (Show, Eq)

data CheckResult = Success CheckMessage |
                   Failure CheckMessage |
                   Exception CheckMessage |
                   Skipped CheckMessage
                   deriving (Show, Eq)

data Check v = Check {
  message :: CheckMessage,
  msgType :: v -> CheckResult
}


type CheckFunc a = String -> a -> Check a
-- runconfig and timedetails should be added to apState -> valState
-- if needed for validation
--

-- accum :: a -> (Bool, [CheckResult]) -> Check a  -> (Bool, [CheckResult])
-- accum valState (excpt, rs) ck = let

isExcption :: CheckResult -> Bool
isExcption = \case
               Exception a -> True
               _ -> False

calcChecks :: forall v. v -> [Check v] -> [CheckResult]
calcChecks vs chks = let
                      iResult :: forall a. (Bool, a) -> Check v ->  CheckResult
                      iResult (excpt, rs) ck  = excpt ? Skipped (message (ck :: Check v)) $ msgType ck vs

                      foldfunc :: (Bool, [CheckResult]) -> Check v ->  (Bool, [CheckResult])
                      foldfunc tpl@(hasEx, lstCr) ck = let
                                                          thisChkR = iResult tpl ck
                                                        in
                                                          (hasEx || isExcption thisChkR, thisChkR : lstCr)
                      in
                       snd $ foldl' foldfunc (False, []) chks


-- chk :: String -> (v -> Bool) -> (v -> Check v)
-- chk msg prd val = let
--                     msgType = prd val ? Success $ Failure
--                   in
--                     msgType
