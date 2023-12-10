module LogListCheck where

import           Data.DList as DL
import           PyrethrumExtras.Test
import List.Extra as L
import Text.Extra as T

type TxtLog = DList Text

chkLog :: (TxtLog -> v) -> (v -> Assertion) -> TxtLog -> Assertion
chkLog intprt assrt = assrt . intprt

chkMessageInstances :: Text -> Int -> DList Text -> Assertion
chkMessageInstances msg exCount  = chkLog (L.count (T.isInfixOf msg) . DL.toList) (chkEq exCount)
