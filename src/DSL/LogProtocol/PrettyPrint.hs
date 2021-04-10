
module DSL.LogProtocol.PrettyPrint (
  prettyPrintLogProtocol,
  prettyPrintLogProtocolWith
) where

import Common as C hiding (Error)
import PrettyPrintCommon as PC
import  DSL.LogProtocol as LP
import           Pyrelude as P
import RunElementClasses as REC
import Check (ResultExpectation(..) , ExpectationActive(..), CheckReport(..), GateStatus(..), classifyResult)

prettyPrintLogProtocolWith :: Show e => Bool -> ThreadInfo -> LogIndex -> Time -> LogProtocolBase e -> Text
prettyPrintLogProtocolWith docMode ThreadInfo{runId, threadIndex} (LogIndex idx) time lgProtocol = 
    runId <>  " - " <> txt threadIndex <> " - " <> txt idx <> " - " <>  txt time
    <> newLn 
    <> prettyPrintLogProtocol docMode lgProtocol

prettyPrintLogProtocol :: Show e => Bool -> LogProtocolBase e -> Text
prettyPrintLogProtocol = prettyPrintLogProtocolBase Nothing

prettyPrintLogProtocolBase :: Show e => Maybe Text -> Bool -> LogProtocolBase e -> Text
prettyPrintLogProtocolBase _mTimeSuffix docMode =
  let
    iterId :: ItemId -> Text
    iterId (ItemId tst iid) = toString tst <> " / item " <> txt iid

    prettyBlock :: Char -> Text -> ItemId -> Text -> Text
    prettyBlock pfxChr headr iid body = indent2 $ toS (replicate 4 ' ') <> singleton pfxChr <> " " <> headr <> " - " <> iterId iid <> newLn <> indent2 body

    ppMsgInfo :: Text -> Text
    ppMsgInfo dtl = null dtl ?
                              "" $
                              newLn <> indent2 (txtPretty dtl)

    docMarkUp :: Text -> Text
    docMarkUp s = (docMode ? id $ indent2) $ (docMode ? "  >> " $ "") <> s

    logIO :: Show a => a -> Text
    logIO m =  docMarkUp $ "IO Action: " <> txtPretty m

    logIO' :: DetailedInfo -> Text
    logIO' (DetailedInfo m i) =  docMarkUp $ "IO Action: " <> txtPretty m <> "\n" <> txtPretty i

    detailDoc :: Text -> DetailedInfo -> Text
    detailDoc hedr (DetailedInfo msg det) = newLn <> (docMode ? id $ indent2) (subHeader hedr <> newLn <> msg <> newLn <> det)                                                                                                  
  in
    \case
        BoundaryLog bl -> case bl of 
                            LP.FilterLog fltrInfos -> ppFilterLog fltrInfos
                            LP.StartRun ttle _offset rc -> ppStartRun ttle rc

                            LP.StartGroup gt -> groupHeader gt
                            LP.EndGroup gt -> groupFooter gt

                            LP.StartTest TestDisplayInfo{..} -> newLn <> tstHeader ("Start Test: " <> toString testModAddress <> " - " <> testTitle) <> 
                                                              newLn <> "Test Config:" <>
                                                              newLn <> ppAesonBlock testConfig

                            EndTest (TestAddress address) -> newLn <> tstHeader ("End Test: " <> address)
                            StartIteration iid  _ _ val -> newLn <> subHeader ("Start Iteration: " <> iterId iid) <> 
                                                            newLn <> "Item:" <> 
                                                            newLn <> ppAesonBlock val <>
                                                            (docMode ? "" $ newLn)

                            EndIteration iid -> newLn <> subHeader ("End Iteration: " <> iterId iid)
                            LP.EndRun -> newLn <> PC.header "End Run"

        -- IterationLog (Doc dp) -> case dp of 
        --                       DocAction ai -> case ai of
        --                         ActionInfo msg -> "  >> " <> msg
        --                         ActionInfo' msg extended -> "  >> " <> 
        --                                                         msg <> 
        --                                                         newLn <> 
        --                                                         indent2 extended
        --                       DocInteraction -> newLn <> "Interaction:"
        --                       DocChecks -> newLn <> "Checks:"
        --                       DocCheck _iid chkhdr resultExpectation gateStatus -> 
        --                                   indent2 $ "% " <> chkhdr  <> 
        --                                       (
        --                                         gateStatus == GateCheck 
        --                                           ? "(Gate: subsequent checks will not be executed if this check fails)" 
        --                                           $ ""
        --                                       ) <> 
        --                                       (
        --                                       case resultExpectation of 
        --                                         ExpectPass -> ""
        --                                         ExpectFailure Inactive  _  -> ""
        --                                         ExpectFailure Active message -> newLn <> indent2 ("!! This check is expected to fail: " <> message)
        --                                       )

        --                       DocIOAction m -> logIO m

        --                       DocMessage s -> docMarkUp $ "message: " <> s
        --                       DocMessage' detailedInfo -> detailDoc "Message" detailedInfo
            
        --                       DocWarning s -> docMarkUp $ "warning: " <> s
        --                       DocWarning' detailedInfo -> detailDoc "Warning" detailedInfo

        --                       e@(DocError _) -> txtPretty e

        StartInteraction -> newLn <> "Interaction:"
        StartChecks -> newLn <> "Checks:"
        StartParser -> newLn <> "Domain:"

        IOAction m -> indent2 $ logIO m 
        IOAction' i -> logIO' i
        
        InteractorSuccess iid (ApStateJSON as) -> newLn <> prettyBlock '>' "Interactor Complete" iid (prettyYamlKeyValues 2 LeftJustify as)
          
        InteractorFailure iid err -> prettyBlock '>' "Interactor Failure" iid $ txtPretty err

        ParserSuccess iid (DStateJSON ds) -> prettyBlock '>' "ParseComplete" iid $ prettyYamlKeyValues 2 LeftJustify ds
        ParserSkipped iid -> docMarkUp $ "ParseSkipped: " <> txt iid
        ParserFailure iid err -> prettyBlock '>' "ParseFailure" iid $ txtPretty err

        CheckOutcome iid (CheckReport reslt (DetailedInfo chkhdr info)) -> prettyBlock 'x' ("Check: " <> txtPretty (classifyResult reslt)) iid $ 
                                                                                                    chkhdr  <> " -> " <> txtPretty reslt <> 
                                                                                                    ppMsgInfo info
        Message s -> docMarkUp $ "message: " <> s
        Message' detailedInfo -> detailDoc "Message" detailedInfo

        Warning s -> docMarkUp $ "warning: " <> s
        Warning' detailedInfo -> detailDoc "Warning" detailedInfo

        e@(Error _) -> txtPretty e
