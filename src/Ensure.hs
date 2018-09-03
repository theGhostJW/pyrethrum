
module Ensure where

import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Error
import qualified Control.Monad as Monad

data Ensure e r where
 Ensure :: Bool -> e -> Ensure e ()
 Fail :: e -> Ensure e ()

ensure :: Member (Ensure e) effs => Bool -> e -> Eff effs ()
ensure condition err = send $ Ensure condition err

fail :: Member (Ensure e) effs => e -> Eff effs ()
fail = send . Fail

-- ensureInterpreterPlus :: (Data.OpenUnion.Internal.FindElem (Error e1) (Error e2 : effs),
--       Data.OpenUnion.Internal.IfNotFound
--         (Error e1) (Error e2 : effs) (Error e2 : effs)) =>
--      Eff (Ensure e1 : Error e2 : effs) a -> Eff effs (Either e2 a)
-- ensureInterpreterPlus effs = runError $ ensureInterpreter effs

ensureInterpreter :: forall effs a. forall e. Member (Error e) effs => Eff (Ensure e ': effs) a -> Eff effs a
ensureInterpreter = interpret $ \case
                                    Ensure condition e -> Monad.unless condition $ throwError e
                                    Fail e -> throwError e
