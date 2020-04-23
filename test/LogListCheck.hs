module LogListCheck where

import           Pyrelude   as P
import           Data.DList
import           Pyrelude.Test
import           DSL.LogProtocol

type TxtLog = DList Text

chkLog :: (TxtLog -> v) -> (v -> Assertion) -> TxtLog -> Assertion
chkLog intprt assrt = assrt . intprt

chkMessageInstances :: Text -> Int -> DList Text -> Assertion
chkMessageInstances msg exCount  = chkLog (P.count (isInfixOf msg)) (chkEq exCount)

type Log = [LogProtocolBase Int]
