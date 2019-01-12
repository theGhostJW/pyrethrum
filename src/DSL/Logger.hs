
module DSL.Logger where

import DSL.Common
import  DSL.LogProtocol
import           Foundation.List.DList
import           Foundation.Extended
import           Foundation.String as S
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import Text.Show.Pretty as PP
import TestAndRunConfig
import qualified Prelude as P
import System.IO

data Logger r where
 LogItem :: LogProtocol a -> Logger ()

logItem :: Member Logger effs => LogProtocol a -> Eff effs ()
logItem = send . LogItem

simpleLog :: forall s a effs. (Show s, Member Logger effs) => (String -> LogProtocol a) -> s -> Eff effs ()
simpleLog lpCons = logItem . lpCons . showPretty

detailLog :: forall a s effs. (Show s, Member Logger effs) => (DetailedInfo -> LogProtocol a) -> s -> s -> Eff effs ()
detailLog lpCons msg additionalInfo = logItem $ lpCons $ prtyInfo msg additionalInfo

log :: forall s effs. (Show s, Member Logger effs) => s -> Eff effs ()
log = simpleLog Message

log' :: forall s effs. (Show s,  Member Logger effs) => s -> s -> Eff effs ()
log' = detailLog  Message'

logWarning :: forall s effs. (Show s, Member Logger effs) => s -> Eff effs ()
logWarning = simpleLog Warning

logWarning' :: forall s effs. (Show s, Member Logger effs) => s -> s -> Eff effs ()
logWarning' = detailLog  Warning'

logError :: forall s effs. (Show s, Member Logger effs) => s -> Eff effs ()
logError = simpleLog (Error . UserError)

logError' :: forall s effs. (Show s, Member Logger effs) => s -> s -> Eff effs ()
logError' = detailLog  (Error . UserError')

logConsoleInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsoleInterpreter = interpretM $ \(LogItem lp) -> P.print lp

logDocInterpreter :: Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocInterpreter = interpret $ \(LogItem lp) -> tell $ dList lp

showPretty :: Show a => a -> String
showPretty = toStr . ppShow

prtyInfo :: (Show s, Show s1)  => s -> s1 -> DetailedInfo
prtyInfo msg adInfo = Info (showPretty msg) (showPretty adInfo)

putLines :: Handle -> String -> IO ()
putLines hOut s = P.sequence_ $ hPutStrLn hOut . toList <$> S.lines s

prettyPrintFilterItem :: TestConfigClass tc => Either (FilterRejection tc) tc -> String
prettyPrintFilterItem =
    let
      description :: TestConfigClass cfg => cfg -> String
      description cnfg = moduleAddress cnfg <> " - " <> title cnfg
    in
      either
        (\r -> "rejected: " <> description (cfg r) <> " - Reason: " <> reason r)
        (\cfg -> "accepted: " <> description cfg)


logString :: LogProtocol a -> String
logString =
            let
              hdr l h = l <> " " <> h <> " " <> l
              subHeader = hdr "----"
              header = hdr "===="
              iterId tst iid = tst <> " / item " <> show iid
              newLn = "\n" :: String
            in
              \case
                   IOAction msg -> "IO Action: " <> showPretty msg

                   Message s -> s
                   Message' detailedInfo -> showPretty detailedInfo

                   Warning s -> subHeader "Warning" <> newLn <> s
                   Warning' detailedInfo -> subHeader "Warning" <> newLn <>  showPretty detailedInfo

                   e@(Error _) -> showPretty e
                   FilterLog fltrInfos -> subHeader "Filter Log" <> newLn <>
                                                foldl' (\acc fi -> acc <> fi <> newLn) "" (prettyPrintFilterItem <$> fltrInfos)

                   StartRun rc -> header ("Test Run: " <> title rc) <> newLn <> showPretty rc
                   StartGroup s -> header $ "Group: " <> s

                   StartTest tc -> subHeader ("Start Test: " <> moduleAddress tc <> " - " <> title tc)
                   StartIteration test iid -> subHeader ("Start Iteration: " <> iterId test iid)
                   EndIteration test iid info -> subHeader ("End Iteration: " <> iterId test iid) <> newLn <> info
                   EndRun rc -> header "End Run"

logConsolePrettyInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsolePrettyInterpreter = logToHandlePrettyInterpreter stdout

logToHandlePrettyInterpreter :: LastMember IO effs => Handle -> Eff (Logger ': effs) ~> Eff effs
logToHandlePrettyInterpreter h = interpretM $ \(LogItem lp) -> putLines h $ logString lp

logDocPrettyInterpreter :: Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocPrettyInterpreter = let
                            toDList :: [String] -> DList String
                            toDList = fromList
                          in
                            interpret $ \(LogItem lp) -> tell $ toDList $ lines $ logString lp
