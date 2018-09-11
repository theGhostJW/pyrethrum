
module DSL.Ensure where

import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Coroutine
import           Control.Monad.Freer.Error
import qualified Control.Monad as Monad

data Ensure r where
 Ensure :: Bool -> String -> Ensure ()
 Throw :: String -> Ensure ()

newtype EnsureError = EnsureError String deriving Show

ensure :: Member Ensure effs => Bool -> String -> Eff effs ()
ensure condition err = send $ Ensure condition err

throw :: Member Ensure effs => String -> Eff effs ()
throw = send . Throw

ensureInterpreter :: forall effs a. Member (Error EnsureError) effs => Eff (Ensure ': effs) a -> Eff effs a
ensureInterpreter = interpret $ \case
                                    Ensure condition message -> Monad.unless condition $ throwError $ EnsureError message
                                    Throw message -> throwError $ EnsureError message
