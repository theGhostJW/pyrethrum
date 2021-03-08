
module DSL.Ensure where

import Common
import           Prelude
import qualified Control.Monad as Monad
import Polysemy
import Polysemy.Error as PE
import Data.Text

data Ensure m a where
  Ensure :: Text -> Bool -> Ensure m ()
  Throw :: Text -> Ensure m ()

makeSem ''Ensure

ensureInterpreter :: forall effs a e. Member (Error (FrameworkError e)) effs => Sem (Ensure ': effs) a -> Sem effs a
ensureInterpreter = interpret $ \case
                                    Ensure message condition -> Monad.unless condition . PE.throw . EnsureError $ message
                                    DSL.Ensure.Throw message -> PE.throw $ EnsureError message

-- when documenting actions we do nothing as ensure and throw 
-- are just programming constructs, they don't represent user actions
ensureDocInterpreter :: forall effs a. Sem (Ensure ': effs) a -> Sem effs a
ensureDocInterpreter = interpret $ \case
                                      Ensure _ _ -> pure ()
                                      DSL.Ensure.Throw _ -> pure ()

