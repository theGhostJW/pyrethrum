
module DSL.Ensure where

import Common
import DSL.Logger
import           Pyrelude
import qualified Control.Monad as Monad
import Polysemy
import Polysemy.Error as PE

data Ensure m a where
  Ensure :: Text -> Bool -> Ensure m ()
  Throw :: Text -> Ensure m ()

makeSem ''Ensure

type Ensurable a = Sem '[Ensure, Error EnsureError] a

ensureInterpreter :: forall effs a. Member (Error EnsureError) effs => Sem (Ensure ': effs) a -> Sem effs a
ensureInterpreter = interpret $ \case
                                    Ensure message condition -> Monad.unless condition $ PE.throw $ EnsureError message
                                    DSL.Ensure.Throw message -> PE.throw $ EnsureError message

-- when documenting actions we do nothing as ensure and throw 
-- are just programming constructs, they don't represent user actions
ensureDocInterpreter :: forall effs a. Sem (Ensure ': effs) a -> Sem effs a
ensureDocInterpreter = interpret $ \case
                                      Ensure message condition -> pure ()
                                      DSL.Ensure.Throw message -> pure ()

fullEnsureInterpreter :: Sem '[Ensure, Error EnsureError] a -> Either EnsureError a
fullEnsureInterpreter effs = run . runError $ ensureInterpreter effs

