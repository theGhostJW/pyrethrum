
module DSL where

import Common
import           Pyrelude
import qualified Control.Monad as Monad
import Polysemy
import Polysemy.Error as PE

ensure :: forall effs e. Member (Error (FrameworkError e)) effs => Text -> Bool ->  Sem effs ()
ensure message condition = Monad.unless condition . PE.throw . EnsureError $ message
