
module DSL.LogProtocol.PrettyPrint (
  prettyPrintLogProtocol,
  prettyPrintLogProtocolWith,
  -- prettyPrintLogProtocolSimple,
  LogStyle(..)
) where

import Common as C ( DetailedInfo(DetailedInfo) )
import PrettyPrintCommon as PC
    ( Justification(LeftJustify),
      newLn,
      indent2,
      subHeader,
      header,
      tstHeader,
      groupHeader,
      groupFooter,
      ppAesonBlock,
      ppStartRun,
      ppFilterLog,
      prettyYamlKeyValues )
import DSL.LogProtocol as LP
    ( LogProtocolBase(Error, FilterLog, StartRun, StartGroup, EndGroup,
                      StartTest, EndTest, StartIteration, EndIteration, EndRun,
                      StartInteraction, StartChecks, StartParser, IOAction, IOAction',
                      InteractorSuccess, InteractorFailure, ParserSuccess, ParserSkipped,
                      ParserFailure, CheckOutcome, Message, Message', Warning, Warning'),
      ThreadInfo(ThreadInfo, runId, threadIndex),
      LogIndex(LogIndex),
      ItemId(ItemId),
      DStateJSON(DStateJSON),
      ApStateJSON(ApStateJSON) )
import Pyrelude as P
    ( ($),
      Show,
      Semigroup((<>)),
      Bool,
      Char,
      Maybe(Nothing),
      Text,
      Category(id),
      Listy(null),
      replicate,
      txt,
      txtPretty,
      toS,
      (?),
      singleton,
      Time, Eq ((==)) )
import RunElementClasses as REC
    ( TestDisplayInfo(TestDisplayInfo, testConfig, testTitle,
                      testModAddress),
      TestAddress(TestAddress),
      toString )

import Check (ResultExpectation(..) , ExpectationActive(..), CheckReport(..), GateStatus(..), classifyResult)

data LogStyle = Run | Doc | SimpleText deriving Eq

separator' :: LogStyle -> Text
separator' = \case
                Run -> newLn
                Doc -> newLn
                SimpleText -> " !!!! "

prettyPrintLogProtocolWith :: Show e => LogStyle -> ThreadInfo -> LogIndex -> Time -> LogProtocolBase e -> Text
prettyPrintLogProtocolWith style ThreadInfo{runId, threadIndex} (LogIndex idx) time lgProtocol = 
    runId <>  " - " <> txt threadIndex <> " - " <> txt idx <> " - " <>  txt time
    <> separator' style 
    <> prettyPrintLogProtocol style lgProtocol

prettyPrintLogProtocolSimple :: Show e => LogProtocolBase e -> Text
prettyPrintLogProtocolSimple = prettyPrintLogProtocolBase Nothing SimpleText

prettyPrintLogProtocol :: Show e => LogStyle -> LogProtocolBase e -> Text
prettyPrintLogProtocol = prettyPrintLogProtocolBase Nothing

prettyPrintLogProtocolBase :: Show e => Maybe Text -> LogStyle -> LogProtocolBase e -> Text
prettyPrintLogProtocolBase _mTimeSuffix style =
  let  
    separator :: Text
    separator = separator' style

    iterId :: ItemId -> Text
    iterId (ItemId tst iid) = toString tst <> " / item " <> txt iid

    prettyBlock :: Char -> Text -> ItemId -> Text -> Text
    prettyBlock pfxChr headr iid body = indent2 $ toS (replicate 4 ' ') <> singleton pfxChr <> " " <> headr <> " - " <> iterId iid <> separator <> indent2 body

    ppMsgInfo :: Text -> Text
    ppMsgInfo dtl = null dtl ?
                              "" $
                              separator <> indent2 (txtPretty dtl)

    docMarkUp :: Text -> Text
    docMarkUp s = (style == Doc ? id $ indent2) $ (style == Doc ? "  >> " $ "") <> s

    logIO :: Show a => a -> Text
    logIO m =  docMarkUp $ "IO Action: " <> txtPretty m

    logIO' :: DetailedInfo -> Text
    logIO' (DetailedInfo m i) = docMarkUp $ "IO Action: " <> txtPretty m <> "\n" <> txtPretty i

    detailDoc :: Text -> DetailedInfo -> Text
    detailDoc hedr (DetailedInfo msg det) = separator <> (style == Doc ? id $ indent2) (subHeader hedr <> separator <> msg <> separator <> det)                                                                                                  
  in
    \case
        LP.FilterLog fltrInfos -> ppFilterLog fltrInfos
        LP.StartRun ttle _offset rc -> ppStartRun ttle rc

        LP.StartGroup gt -> groupHeader gt
        LP.EndGroup gt -> groupFooter gt

        LP.StartTest TestDisplayInfo{..} -> separator <> tstHeader ("Start Test: " <> toString testModAddress <> " - " <> testTitle) <> 
                                          separator <> "Test Config:" <>
                                          separator <> ppAesonBlock testConfig

        EndTest (TestAddress address) -> separator <> tstHeader ("End Test: " <> address)
        StartIteration iid  _ _ val -> separator <> subHeader ("Start Iteration: " <> iterId iid) <> 
                                        separator <> "Item:" <> 
                                        separator <> ppAesonBlock val <>
                                        (style == Doc ? "" $ separator)

        EndIteration iid -> separator <> subHeader ("End Iteration: " <> iterId iid)
        LP.EndRun -> separator <> PC.header "End Run"

        -- IterationLog (Doc dp) -> case dp of 
        --                       DocAction ai -> case ai of
        --                         ActionInfo msg -> "  >> " <> msg
        --                         ActionInfo' msg extended -> "  >> " <> 
        --                                                         msg <> 
        --                                                         separator <> 
        --                                                         indent2 extended
        --                       DocInteraction -> separator <> "Interaction:"
        --                       DocChecks -> separator <> "Checks:"
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
        --                                         ExpectFailure Active message -> separator <> indent2 ("!! This check is expected to fail: " <> message)
        --                                       )

        --                       DocIOAction m -> logIO m

        --                       DocMessage s -> docMarkUp $ "message: " <> s
        --                       DocMessage' detailedInfo -> detailDoc "Message" detailedInfo
            
        --                       DocWarning s -> docMarkUp $ "warning: " <> s
        --                       DocWarning' detailedInfo -> detailDoc "Warning" detailedInfo

        --                       e@(DocError _) -> txtPretty e

        StartInteraction -> separator <> "Interaction:"
        StartChecks -> separator <> "Checks:"
        StartParser -> separator <> "Domain:"

        IOAction m -> indent2 $ logIO m 
        IOAction' i -> logIO' i
        
        InteractorSuccess iid (ApStateJSON as) -> separator <> prettyBlock '>' "Interactor Complete" iid (prettyYamlKeyValues 2 LeftJustify as)
          
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
