module LogListCheck where

import           Data.DList as DL
import           PyrethrumExtras.Test
import PyrethrumExtras as PE
import Data.Text as T

type TxtLog = DList Text

chkLog :: (TxtLog -> v) -> (v -> Assertion) -> TxtLog -> Assertion
chkLog intprt assrt = assrt . intprt

chkMessageInstances :: Text -> Int -> DList Text -> Assertion
chkMessageInstances msg exCount  = chkLog (PE.count (T.isInfixOf msg) . DL.toList) (chkEq exCount)
