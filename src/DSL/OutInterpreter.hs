-- TODO - Why do I to need this?
{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.OutInterpreter (
  runOut
) where

import DSL.OutEffect as OE
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (StaticRep, SideEffects(..), getStaticRep, unsafeEff_, evalStaticRep)

runOut :: (IOE :> es) => (a -> IO ()) -> Eff (Out a : es) b -> Eff es b
runOut = evalStaticRep . Out . Sink
