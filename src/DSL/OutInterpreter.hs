-- TODO - Why do I to need this?
{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module DSL.OutInterpreter (
  runOut
) where

import DSL.OutEffect as OE
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Static (evalStaticRep)

runOut :: (IOE :> es, NFData a) => (a -> IO ()) -> Eff (Out a : es) b -> Eff es b
runOut sink = 
  evalStaticRep . Out $ Sink forcedSink
  where 
    forcedSink = sink . force
