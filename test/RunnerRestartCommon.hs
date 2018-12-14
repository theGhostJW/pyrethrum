module RunnerRestartCommon where

import qualified Check                 as C
import           Foundation.Extended   as F
import           Foundation.List.DList
import           Test.Extended         as U

type Log = DList String

chkLog :: (Log -> v) -> (v -> Assertion) -> Log -> Assertion
chkLog intprt assrt = assrt . intprt

chkMessageInstances :: String -> Int -> DList String -> Assertion
chkMessageInstances msg exCount  = chkLog (count (isInfixOf msg)) (chkEq exCount)
