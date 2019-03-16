
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

simpleLog :: forall effs. Member Logger effs => (String -> LogProtocol) -> String -> Eff effs ()
simpleLog lpCons = logItem . lpCons

detailLog :: forall effs. Member Logger effs => (DetailedInfo -> LogProtocol) -> String -> String -> Eff effs ()
detailLog lpCons msg additionalInfo = logItem $ lpCons $ DetailedInfo msg additionalInfo

log :: forall effs. Member Logger effs => String -> Eff effs ()
log = simpleLog Message

log' :: forall effs. Member Logger effs => String -> String -> Eff effs ()
log' = detailLog  Message'

logWarning :: forall effs. Member Logger effs => String -> Eff effs ()
logWarning = simpleLog Warning

logWarning' :: forall effs. (Member Logger effs) => String -> String -> Eff effs ()
logWarning' = detailLog  Warning'

logError :: forall effs.  Member Logger effs => String -> Eff effs ()
logError = simpleLog (Error . AppUserError)

logError' :: forall effs. Member Logger effs => String -> String -> Eff effs ()
logError' = detailLog  (Error . AppUserError')

logConsoleInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsoleInterpreter = interpretM $ \(LogItem lp) -> P.print lp

logDocInterpreter :: Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs
logDocInterpreter = interpret $ \(LogItem lp) -> tell $ dList lp

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

logStrPP :: Bool -> LogProtocol -> String
logStrPP docMode =
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
              prettyBlock pfxChr headr iid body = indent2 $ F.replicate (CountOf 3) pfxChr <> " " <> headr <> " - " <> iterId iid <> newLn <> indent2 body

              ppAeson:: Y.Value -> String
              ppAeson val = toS ((getLenient . toS . Y.encode $ val) :: T.Text)

              ppAesonBlock:: Y.Value -> String
              ppAesonBlock = indent2 . ppAeson

              ppMsgInfo :: Show a => Maybe a -> String
              ppMsgInfo mbInfo = maybef mbInfo
                                        ""
                                        (\m -> newLn <> indent2 (showPretty m))

              docMarkUp :: String -> String
              docMarkUp s = (docMode ? id $ indent2) $ (docMode ? "  >> " $ "") <> s

              logIO :: Show a => a -> String
              logIO m =  docMarkUp $ "IO Action: " <> showPretty m

              detailDoc :: String -> DetailedInfo -> String
              detailDoc hedr (DetailedInfo msg det) = newLn <> (docMode ? id $ indent2) (subHeader hedr <> newLn <> msg <> newLn <> det)
                                                                                                               
            in
              \case
                  StartInteraction -> newLn <> "Interaction:"
                  StartChecks -> newLn <> "Checks:"
                  StartPrepState -> newLn <> "PrepState:"

                  DocAction ai -> case ai of
                                      ActionInfo msg -> "  >> " <> msg
                                      ActionInfoM msg extended -> "  >> " <> 
                                                                      msg <> 
                                                                      newLn <> 
                                                                      indent2 extended

                  DocCheck iid chkhdr resultExpectation gateStatus -> indent2 $ "% " <> chkhdr  <> 
                                                              (
                                                                gateStatus == GateCheck 
                                                                  ? "(Gate: subsequent checks will not be executed if this check fails)" 
                                                                  $ ""
                                                              ) <> 
                                                              (
                                                              case resultExpectation of 
                                                                ExpectPass -> ""
                                                                ExpectFailure Inactive  _  -> ""
                                                                ExpectFailure Active message -> newLn <> indent2 ("!! This check is expected to fail: " <> message)
                                                              )
                  
                  DocIOAction m -> logIO m
                  IOAction m -> indent2 $ logIO m

                  Message s -> docMarkUp $ "message: " <> s
                  Message' detailedInfo -> detailDoc "Message" detailedInfo

                  Warning s -> docMarkUp $ "warning: " <> s
                  Warning' detailedInfo -> detailDoc "Warning" detailedInfo

                  InteractorSuccess iid (ApStateDisplay as) -> newLn <> prettyBlock '>' "Interactor Complete"  iid as
                    
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
                                                  newLn <> ppAesonBlock val <>
                                                  (docMode ? "" $ newLn)

                  EndIteration iid -> newLn <> subHeader ("End Iteration: " <> iterId iid)
                  EndRun -> newLn <> header "End Run"

logConsolePrettyInterpreter :: LastMember IO effs => Eff (Logger ': effs) ~> Eff effs
logConsolePrettyInterpreter = logToHandles [(logStrPP False, stdout)]

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
                            interpret $ \(LogItem lp) -> tell . toDList . lines $ logStrPP True lp
