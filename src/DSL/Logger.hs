
module DSL.Logger where

import DSL.Common
import  DSL.LogProtocol
import           Foundation.Extended
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import qualified Prelude as P

data Logger r where
 LogItem :: (Show rc, Show tc) => LogProtocol rc tc -> Logger ()

logItem :: (Show rc, Show tc, Member Logger effs) => LogProtocol rc tc -> Eff effs ()
logItem = send . LogItem

log :: (Show s, Member Logger effs) => s -> Eff effs ()
log = logItem . Message . show

log' :: (Show s, Member Logger effs) => s -> s -> Eff effs ()
log' msg additionalInfo = logItem $  Message' $ Info (show msg) (show additionalInfo)

logWaring :: (Show s, Member Logger effs) => s -> Eff effs ()
logWaring = logItem . Warning . show

logWarning' :: (Show s, Member Logger effs) => s -> s -> Eff effs ()
logWarning' msg additionalInfo = logItem $  Warning' $ Info (show msg) (show additionalInfo)

logError :: (Show s, Member Logger effs) => s -> Eff effs ()
logError = logItem . Error . UserError . show

logError' :: (Show s, Member Logger effs) => s -> s -> Eff effs ()
logError' msg additionalInfo = logItem $  Error $ UserError' $ Info (show msg) (show additionalInfo)


logConsoleInterpreter :: LastMember IO effs => Eff (Logger ': effs) a -> Eff effs a
logConsoleInterpreter = interpretM $ \(LogItem lp) -> P.print lp

logDocInterpreter :: Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocInterpreter = interpret $ \(LogItem lp) -> tell $ dList lp
