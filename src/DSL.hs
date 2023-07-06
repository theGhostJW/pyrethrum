module DSL (
  ensure,
  -- module DSL.FileSystemDocInterpreter,
  -- module DSL.FileSystemEffect,
  -- module DSL.FileSystemIOInterpreter,
  -- module DSL.Out,
  -- module DSL.Internal.ApEvent,
) where

import Common (FrameworkError (EnsureError))
import qualified Control.Monad as Monad
import Polysemy (Member, Sem)
import Polysemy.Error as PE (Error, throw)

ensure :: forall effs e. (Member (Error (FrameworkError e)) effs) => Text -> Bool -> Sem effs ()
ensure message condition = Monad.unless condition . PE.throw . EnsureError $ message
