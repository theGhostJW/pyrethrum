
module DSL.Ensure where

import Common
import DSL.Logger
import           Pyrelude
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import qualified Control.Monad as Monad

data Ensure r where
  Ensure :: Text -> Bool -> Ensure ()
  Throw :: Text -> Ensure ()

type Ensurable a = Eff '[Ensure, Error EnsureError] a

ensure :: Member Ensure effs => Text -> Bool -> Eff effs ()
ensure err condition = send $ Ensure err condition

throw :: Member Ensure effs => Text -> Eff effs ()
throw = send . Throw

ensureInterpreter :: forall effs a. Member (Error EnsureError) effs => Eff (Ensure ': effs) a -> Eff effs a
ensureInterpreter = interpret $ \case
                                    Ensure message condition -> Monad.unless condition $ throwError $ EnsureError message
                                    Throw message -> throwError $ EnsureError message

-- when documenting actions we do nothing as ensure and throw 
-- are just programming constructs, they don't represent user actions
ensureDocInterpreter :: forall effs a. Eff (Ensure ': effs) a -> Eff effs a
ensureDocInterpreter = interpret $ \case
                                    Ensure message condition -> pure ()
                                    Throw message -> pure ()

fullEnsureInterpreter :: Eff '[Ensure, Error EnsureError] a -> Either EnsureError a
fullEnsureInterpreter effs = run $ runError $ ensureInterpreter effs

