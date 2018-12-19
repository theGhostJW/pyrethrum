
module DSL.Ensure where

import DSL.Common
import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import qualified Control.Monad as Monad

data Ensure r where
  Ensure :: Truthy conditon => String -> conditon -> Ensure ()
  Throw :: String -> Ensure ()

type Ensurable a = Eff '[Ensure, Error EnsureError] a


ensure :: Member Ensure effs => String -> Bool -> Eff effs ()
ensure err condition = send $ Ensure err condition

throw :: Member Ensure effs => String -> Eff effs ()
throw = send . Throw

ensureInterpreter :: forall effs a. Member (Error EnsureError) effs => Eff (Ensure ': effs) a -> Eff effs a
ensureInterpreter = interpret $ \case
                                    Ensure message condition -> Monad.unless (isTruthy condition) $ throwError $ EnsureError message
                                    Throw message -> throwError $ EnsureError message

fullEnsureInterpreter :: Eff '[Ensure, Error EnsureError] a -> Either EnsureError a
fullEnsureInterpreter effs = run $ runError $ ensureInterpreter effs
