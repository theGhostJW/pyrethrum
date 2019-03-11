
module DSL.Logger where

import Common
import  DSL.LogProtocol
import           Foundation.List.DList
import           Foundation.Extended as F
import           Foundation.String as S
import           Control.Monad.Freer
import           Control.Monad.Freer.Writer
import Text.Show.Pretty as PP
import RunElementClasses as C
import qualified Prelude as P
import System.IO
--import           RunnerBase
import qualified Data.Aeson as A
import Foundation.Compat.ByteString
import qualified Data.ByteString.Lazy as B
import Check
import Data.Yaml as Y
import qualified Data.Text as T
import Basement.String as S

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
showPretty = toS . ppShow

indentString :: Int -> String -> String
indentString i s = 
  let 
    linesClean :: [String]
    linesClean = fst . F.breakEnd (not . S.all (' ' ==)) $ S.lines s

    unlined :: P.String
    unlined = P.unlines $ (\s' -> s == "" ? "" $ toS $ F.replicate (CountOf i) ' ' <> s')  <$> linesClean
  in 
    toS $ safeLast unlined == Just '\n' ? P.init unlined $ unlined  

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

              iterId :: ItemId -> String
              iterId (ItemId tst iid) = toString tst <> " / item " <> show iid

              newLn :: String
              newLn = "\n" :: String

              indent2 :: String -> String
              indent2 = indentString 2 

              prettyBlock :: Char -> String -> ItemId -> String -> String
              prettyBlock pfxChr headr iid body = newLn <> F.replicate (CountOf 3) pfxChr <> " " <> headr <> " - " <> iterId iid <> newLn <> indent2 body

              ppAeson:: Y.Value -> String
              ppAeson val = toS ((getLenient . toS . Y.encode $ val) :: T.Text)

              ppAesonBlock:: Y.Value -> String
              ppAesonBlock = indent2 . ppAeson

              ppMsgInfo :: Show a => Maybe a -> String
              ppMsgInfo mbInfo = maybef mbInfo
                                        ""
                                        (\m -> newLn <> indent2 (showPretty m))

              logIO :: Show a => a -> String
              logIO m = "IO Action: " <> showPretty m
                                                                                                               
            in
              \case
                  DocAction ai -> case ai of
                                      ActionInfo msg -> ">> " <> msg
                                      ActionInfoM msg extended -> ">> " <> 
                                                                      msg <> 
                                                                      newLn <> 
                                                                      indent2 extended

                  DocCheck iid chkhdr resultExpectation gateStatus -> "check -> " <> showPretty chkhdr  <> 
                                                              (
                                                                gateStatus == GateCheck 
                                                                  ? " - * Gate (subsequent checks will not be executed if this check fails)" 
                                                                  $ ""
                                                              ) <> 
                                                              (
                                                              case resultExpectation of 
                                                                ExpectPass -> ""
                                                                ExpectFailure _ Inactive -> ""
                                                                ExpectFailure message Active -> newLn <> indent2 ("This check is expected to fail: " <> message)
                                                              )
                  
                  DocIOAction m -> logIO m
                  IOAction m -> logIO m

                  Message s -> "USER MSG >>" <> s
                  Message' detailedInfo -> showPretty detailedInfo

                  Warning s -> subHeader "Warning" <> newLn <> s
                  Warning' detailedInfo -> subHeader "Warning" <> newLn <> showPretty detailedInfo

                  InteractorSuccess iid (ApStateDisplay as) -> prettyBlock '>' "Interactor Complete"  iid as
                    
                  InteractorFailure iid err -> prettyBlock '>' "Interactor Failure" iid $ showPretty err

                  PrepStateSuccess iid (DStateDisplay ds) -> prettyBlock '>' "PrepState Complete" iid ds
                  PrepStateFailure iid err -> prettyBlock '>' "PrepState Failure" iid $ showPretty err

                  CheckOutcome iid (CheckReport reslt (CheckInfo chkhdr mbInfo)) -> prettyBlock 'x' ("Check: " <> showPretty (classifyResult reslt)) iid $ 
                                                                                                               chkhdr  <> " -> " <> showPretty reslt <> 
                                                                                                               ppMsgInfo mbInfo
                  e@(Error _) -> showPretty e
                  FilterLog fltrInfos -> newLn <> header "Filter Log" <> newLn <>
                                                foldl' (\acc fi -> acc <> fi <> newLn) "" (prettyPrintFilterItem <$> fltrInfos)

                  StartRun ttle rc -> header ("Test Run: " <> unRunTitle ttle) <> 
                                       newLn <> "Run Config:" <>
                                       newLn <> ppAesonBlock rc

                  StartGroup gt -> header $ "Group: " <> unGroupTitle gt
                  EndGroup gt -> header $ "End Group: " <> unGroupTitle gt

                  StartTest TestDisplayInfo{..} -> newLn <> tstHeader ("Start Test: " <> toString testModAddress <> " - " <> testTitle) <> 
                                                    newLn <> "Test Config:" <>
                                                    newLn <> ppAesonBlock testConfig

                  EndTest (TestModule address) -> newLn <> tstHeader ("End Test: " <> address)
                  StartIteration iid  _ _ val -> newLn <> subHeader ("Start Iteration: " <> iterId iid) <> 
                                                  newLn <> "Item:" <> 
                                                  newLn <> ppAesonBlock val

                  EndIteration iid -> newLn <> subHeader ("End Iteration: " <> iterId iid)
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
