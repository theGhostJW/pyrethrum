
module DSL.Logger where

import DSL.Common
import  DSL.LogProtocol
import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import qualified Prelude as P

data Logger r where
 LogItem :: LogProtocol -> Logger ()

logItem :: Member Logger effs => LogProtocol -> Eff effs ()
logItem = send . LogItem

log :: (Show s, Member Logger effs) => s -> Eff effs ()
log = logItem . Message . show


logConsoleInterpreter :: LastMember IO effs => Eff (Logger ': effs) a -> Eff effs a
logConsoleInterpreter =  interpretM $ \case
                                         LogItem lp -> P.print lp

logDocInterpreter :: Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocInterpreter = interpret $ \case
                                    LogItem lp -> tell $ dList lp
