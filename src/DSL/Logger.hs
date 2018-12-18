
module DSL.Logger where

import DSL.Internal.Common
import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import qualified Prelude as P

data Logger r where
 Log :: Show s => s -> Logger ()

log :: (Show s, Member Logger effs) => s -> Eff effs ()
log = send . Log

logConsoleInterpreter :: LastMember IO effs => Eff (Logger ': effs) a -> Eff effs a
logConsoleInterpreter =  interpretM $ \case
                                         Log msg -> P.print msg

logDocInterpreter :: Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocInterpreter = interpret $ \case
                                    Log msg -> tell $ dList msg
