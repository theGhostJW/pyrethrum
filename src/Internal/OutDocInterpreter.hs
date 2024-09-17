module Internal.OutDocInterpreter where

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

-- !!!! This is wrong
-- in doc mode we supress log
runDocOut :: forall a es. (IOE :> es) => Eff (Out NodeEvent : es) a -> Eff es a
runDocOut =
  runOut $ \case
    AE.Framework l -> print l
    AE.User _l -> pure ()
