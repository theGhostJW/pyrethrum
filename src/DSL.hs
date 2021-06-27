
module DSL where

import Common ( FrameworkError(EnsureError) )
import Pyrelude ( ($), Bool, Text, Category((.)) )
import qualified Control.Monad as Monad
import Polysemy ( Member, Sem )
import Polysemy.Error as PE ( Error, throw )

ensure :: forall effs e. Member (Error (FrameworkError e)) effs => Text -> Bool ->  Sem effs ()
ensure message condition = Monad.unless condition . PE.throw . EnsureError $ message
