module LogListCheck where

import           Data.DList as DL
import           PyrethrumExtras.Test
import           DSL.LogProtocol
import List.Extra
import Text.Extra as T

type TxtLog = DList Text

chkLog :: (TxtLog -> v) -> (v -> Assertion) -> TxtLog -> Assertion
chkLog intprt assrt = assrt . intprt

chkMessageInstances :: Text -> Int -> DList Text -> Assertion
chkMessageInstances msg exCount  = chkLog (T.count (T.isInfixOf msg)) (chkEq exCount)

type Log = [LogProtocolBase Int]
