
module DSL.Logger where

import DSL.Common
import  DSL.LogProtocol
import           Foundation.Extended
import           Foundation.String as S
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import Text.Show.Pretty
import TestAndRunConfig
import qualified Prelude as P

showPretty :: Show a => a -> String
showPretty = toStr . ppShow

prtyInfo :: (Show s, Show s1)  => s -> s1 -> DetailedInfo
prtyInfo msg adInfo = Info (showPretty msg) (showPretty adInfo)

data Logger r where
 LogItem :: (Show rc, Show tc, TestConfigClass tc, Titled rc) => LogProtocol rc tc -> Logger ()

logItem :: (Show rc, Show tc, TestConfigClass tc, Titled rc, Member Logger effs) => LogProtocol rc tc -> Eff effs ()
logItem = send . LogItem

simpleLog :: forall s rc tc effs. (Show s, Show rc, Show tc, TestConfigClass tc, Titled rc, Member Logger effs) => (String -> LogProtocol rc tc) -> s -> Eff effs ()
simpleLog lpCons = (logItem :: LogProtocol rc tc -> Eff effs ()) . lpCons . showPretty

detailLog :: forall s rc tc effs. (Show s, Show rc, Show tc, TestConfigClass tc, Titled rc, Member Logger effs) => (DetailedInfo -> LogProtocol rc tc) -> s -> s -> Eff effs ()
detailLog lpCons msg additionalInfo = (logItem :: LogProtocol rc tc -> Eff effs ()) $ lpCons $ prtyInfo msg additionalInfo

log :: forall s rc tc effs. (Show s, Show rc, Show tc, TestConfigClass tc, Titled rc, Member Logger effs) => s -> Eff effs ()
log = (simpleLog :: (String -> LogProtocol rc tc) -> s -> Eff effs ()) Message

log' :: forall s rc tc effs. (Show s, Show rc, Show tc, TestConfigClass tc, Titled rc, Member Logger effs) => s -> s -> Eff effs ()
log' = (detailLog :: (DetailedInfo -> LogProtocol rc tc) -> s -> s -> Eff effs ()) Message'

logWaring :: forall s rc tc effs. (Show s, Show rc, Show tc, TestConfigClass tc, Titled rc, Member Logger effs) => s -> Eff effs ()
logWaring = (simpleLog :: (String -> LogProtocol rc tc) -> s -> Eff effs ()) Warning

logWarning' :: forall s rc tc effs. (Show s, Show rc, Show tc, TestConfigClass tc, Titled rc, Member Logger effs) => s -> s -> Eff effs ()
logWarning' = (detailLog :: (DetailedInfo -> LogProtocol rc tc) -> s -> s -> Eff effs ()) Warning'

logError :: forall s rc tc effs. (Show s, Show rc, Show tc, TestConfigClass tc, Titled rc, Member Logger effs) => s -> Eff effs ()
logError =  (simpleLog :: (String -> LogProtocol rc tc) -> s -> Eff effs ()) (Error . UserError)

logError' :: forall s rc tc effs. (Show s, Show rc, Show tc, TestConfigClass tc, Titled rc, Member Logger effs) => s -> s -> Eff effs ()
logError' = (detailLog :: (DetailedInfo -> LogProtocol rc tc) -> s -> s -> Eff effs ()) (Error . UserError')

logConsoleInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsoleInterpreter = interpretM $ \(LogItem lp) -> P.print lp

logDocInterpreter :: Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocInterpreter = interpret $ \(LogItem lp) -> tell $ dList lp

putLines :: String -> IO ()
putLines s = P.sequence_ $ P.putStrLn . toList <$> S.lines s

ppFilterItem :: Titled tc => Either (FilterRejection tc) tc -> String
ppFilterItem =
    either
      (\r -> "rejected: " <> title (cfg r) <> " - " <> reason r)
      (\cfg -> "accepted: " <> title cfg)


logString :: (Show rc, Show tc, TestConfigClass tc, Titled rc)=> LogProtocol rc tc -> String
logString =
            let
              hdr l h c = l <> " " <> h <> " " <> l <> "\n" <> c
              subHeader = hdr "----"
              header = hdr "===="
            in
              \case
                   Message s -> s
                   Message' detailedInfo -> showPretty detailedInfo

                   Warning s -> subHeader "Warning" s
                   Warning' detailedInfo -> subHeader "Warning" $ showPretty detailedInfo

                   er@(Error e) -> showPretty er
                   FilterLog liFilterInfo -> subHeader "Filter Log" $ foldl' (\acc ip -> acc <> ip <> "\n") "" (ppFilterItem <$> liFilterInfo) 

                   StartRun rc -> undefined
                   StartGroup s -> "==== Group: " <> s <> " ===="

                   StartTest tc -> undefined
                   StartIteration test iid  -> undefined
                   EndIteration test info -> undefined

-- \(LogItem lp) -> P.putStrLn $ ppShow lp
logConsolePrettyInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsolePrettyInterpreter = interpretM $ \(LogItem lp) -> putLines $ logString lp

logDocPrettyInterpreter :: Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocPrettyInterpreter = interpret $ \(LogItem lp) -> tell $ dList $ ppShow lp
