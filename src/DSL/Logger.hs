
module DSL.Logger where

import DSL.Common
import  DSL.LogProtocol
import           Foundation.List.DList
import           Foundation.Extended
import           Foundation.String as S
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import Text.Show.Pretty as PP
import TestAndRunConfig as C
import qualified Prelude as P
import System.IO
import           TestFilter
import           RunnerBase
import  qualified Data.Aeson as A
import Foundation.Compat.ByteString
import qualified Data.ByteString.Lazy as B

data Logger r where
 LogItem :: LogProtocol -> Logger ()

logItem :: Member Logger effs => LogProtocol -> Eff effs ()
logItem = send . LogItem

simpleLog :: forall s effs. (Show s, Member Logger effs) => (String -> LogProtocol) -> s -> Eff effs ()
simpleLog lpCons = logItem . lpCons . showPretty

detailLog :: forall s effs. (Show s, Member Logger effs) => (DetailedInfo -> LogProtocol) -> s -> s -> Eff effs ()
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
logError = simpleLog (Error . AppUserError)

logError' :: forall s effs. (Show s, Member Logger effs) => s -> s -> Eff effs ()
logError' = detailLog  (Error . AppUserError')

logConsoleInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsoleInterpreter = interpretM $ \(LogItem lp) -> P.print lp

logDocInterpreter :: Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocInterpreter = interpret $ \(LogItem lp) -> tell $ dList lp

showPretty :: Show a => a -> String
showPretty = toStr . ppShow

prtyInfo :: (Show s, Show s1)  => s -> s1 -> DetailedInfo
prtyInfo msg adInfo = DetailedInfo (showPretty msg) (showPretty adInfo)

putLines :: Handle -> String -> IO ()
putLines hOut s = P.sequence_ $ hPutStrLn hOut . toList <$> S.lines s

prettyPrintFilterItem :: FilterResult -> String
prettyPrintFilterItem FilterResult{..} =
    let
      description :: String
      description = toString (testModAddress testInfo) <> " - " <> testTitle testInfo
    in
      maybef reasonForRejection
        ("accepted: " <> description)
        (\reason -> "rejected: " <> description <> " - Reason: " <> reason)

logStrJSON :: LogProtocol -> String
logStrJSON = fst . fromBytesLenient . fromByteString . B.toStrict . A.encode

logStrPP :: LogProtocol -> String
logStrPP =
            let
              hdr l h = l <> " " <> h <> " " <> l
              subHeader = hdr "----"
              header = hdr "===="
              tstHeader = hdr "==="
              itrHeader = hdr "=="
              iterId tst iid = toString tst <> " / item " <> show iid
              newLn = "\n" :: String
            in
              \case
                   IOAction msg -> "IO Action: " <> showPretty msg

                   Message s -> s
                   Message' detailedInfo -> showPretty detailedInfo

                   Warning s -> subHeader "Warning" <> newLn <> s
                   Warning' detailedInfo -> subHeader "Warning" <> newLn <>  showPretty detailedInfo

                   e@(Error _) -> showPretty e
                   FilterLog fltrInfos -> newLn <> header "Filter Log" <> newLn <>
                                                foldl' (\acc fi -> acc <> fi <> newLn) "" (prettyPrintFilterItem <$> fltrInfos)

                   StartRun ttle rc -> header ("Test Run: " <> ttle) <> newLn <> showPretty rc
                   StartGroup s -> header $ "Group: " <> s
                   EndGroup s -> header $ "End Group: " <> s


                   StartTest TestDisplayInfo{..} -> newLn <> tstHeader ("Start Test: " <> toString testModAddress <> " - " <> testTitle)
                   EndTest TestDisplayInfo{..} -> tstHeader ("End Test: " <> toString testModAddress <> " - " <> testTitle)
                   StartIteration test iid _ -> newLn <> subHeader ("Start Iteration: " <> iterId test iid)

                   Result test iid rsltInfo -> newLn <> subHeader ("Result: " <> iterId test iid) <> newLn <> rsltInfo
                   EndIteration test iid -> subHeader $ "End Iteration: " <> iterId test iid
                   EndRun -> newLn <> header "End Run"

logConsolePrettyInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsolePrettyInterpreter = logToHandles [(logStrPP, stdout)]

logToHandles :: LastMember IO effs => [(LogProtocol -> String, Handle)] -> Eff (Logger ': effs) ~> Eff effs
logToHandles convertersHandlers = 
                        let 
                          logToHandle :: (LogProtocol -> String) -> Handle -> Logger r -> IO ()
                          logToHandle lp2Str h (LogItem lp) = putLines h $ lp2Str lp
                        in
                          interpretM $ \li@(LogItem lp) -> 
                                        let 
                                          logToh :: (LogProtocol -> String, Handle) -> IO ()
                                          logToh (f, h) = logToHandle f h li
                                        in
                                          P.sequence_ $ logToh <$> convertersHandlers
                             
logDocPrettyInterpreter :: Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocPrettyInterpreter = let
                            toDList :: [String] -> DList String
                            toDList = fromList
                          in
                            interpret $ \(LogItem lp) -> tell . toDList . lines $ logStrPP lp
