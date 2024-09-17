module Internal.OutIOInterpreter where

-- Import necessary modules
import Data.Text (Text)
import Effectful as EF
  ( Eff,
    IOE,
    runEff,
    type (:>),
  )
import DSL.Internal.NodeEvent as AE
import DSL.Out

-- Define your functions and types here

-- TODO - interpreters into own module
-- Need to fix up to work in with logcontrols
-- there are currently 2 paths to STD out I think ??
-- this is wrong
runIOOut :: forall a es. (IOE :> es) => Eff (Out NodeEvent : es) a -> Eff es a
runIOOut = runOut print
