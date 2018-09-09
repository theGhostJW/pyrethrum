
module DSL.Ensure where

import AppError
import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Error
import qualified Control.Monad as Monad

data Ensure r where
 Ensure :: Bool -> String -> Ensure ()
 Fail :: String -> Ensure ()

ensure :: Member Ensure effs => Bool -> String -> Eff effs ()
ensure condition err = send $ Ensure condition err

fail :: Member Ensure effs => String -> Eff effs ()
fail = send . Fail

ensureInterpreter :: forall effs a. Member (Error AppError) effs => Eff (Ensure ': effs) a -> Eff effs a
ensureInterpreter = interpret $ \case
                                    Ensure condition message -> Monad.unless condition $ throwError $ EnsureError message
                                    Fail message -> throwError $ EnsureError message
