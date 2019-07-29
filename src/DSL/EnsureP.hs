
module DSL.EnsureP where

import Common
import DSL.Logger
import           Pyrelude
-- import           Control.Monad.Freer
-- import           Control.Monad.Freer.Error
import qualified Control.Monad as Monad
import Polysemy
import Polysemy.Error

data Ensure m a where
  Ensure :: Text -> Bool -> Ensure m ()
  Throw :: Text -> Ensure m ()

makeSem ''Ensure

-- type Ensurable m a = Sem '[Error EnsureError] a

-- ensureInterpreter :: forall effs a. Member (Error EnsureError) effs => Sem (Ensure ': effs) a -> Sem effs a
-- ensureInterpreter = interpret $ \case
--                                     Ensure message condition -> Monad.unless condition $ throwError $ EnsureError message
--                                     DSL.EnsureP.Throw message -> throwError $ EnsureError message

-- -- when documenting actions we do nothing as ensure and throw 
-- -- are just programming constructs, they don't represent user actions
-- ensureDocInterpreter :: forall effs a. Sem (Ensure ': effs) a -> Sem effs a
-- ensureDocInterpreter = interpret $ \case
--                                     Ensure message condition -> pure ()
--                                     DSL.EnsureP.Throw message -> pure ()

-- fullEnsureInterpreter :: Sem '[Ensure, Error EnsureError] a -> Either EnsureError a
-- fullEnsureInterpreter effs = run $ runError $ ensureInterpreter effs

