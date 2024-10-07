module DSL.Logging (
 log,
 logTxt
) where

import DSL.OutEffect
import DSL.Internal.NodeLog qualified as E
import Effectful as EF
  ( Eff,
    type (:>),
  )
import PyrethrumExtras (txt)

{- TODO Other efect funtions such as warning and folder -}

logTxt :: (Out E.NodeLog :> es, Show a) => a -> Eff es ()
logTxt = log . txt

log :: (Out E.NodeLog :> es) => Text -> Eff es ()
log = out . E.User . E.Info