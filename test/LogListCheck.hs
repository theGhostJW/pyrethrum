module LogListCheck where

import           Pyrelude   as P
import           Data.DList
import           Pyrelude.Test         as U
import           Pyrelude.Data.Text.Hidden         as H

type Log = DList Text

chkLog :: (Log -> v) -> (v -> Assertion) -> Log -> Assertion
chkLog intprt assrt = assrt . intprt

chkMessageInstances :: Text -> Int -> DList Text -> Assertion
chkMessageInstances msg exCount  = chkLog (P.count (H.isInfixOf msg)) (chkEq exCount)
