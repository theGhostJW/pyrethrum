module LogTransformation.PrintLogDisplayElement (
  emptyIterationAccum,
  emptyProbleIterationAccum,
  printLogDisplayStep,
  printProblemsDisplayStep,
  prettyPrintDisplayElement,
  IterationAccum(..),
  PrintLogDisplayElement(..),
  IterationRecord(..),
  LogTransformError(..),
  IterationError(..),
  IterationWarning(..),
  ApStateInfo(..),
  PrepStateInfo(..)
) where

import Common as C (FrameworkError(..), indentText)
import LogTransformation.Common as LC
import Check as CK
import Pyrelude as P
import qualified DSL.LogProtocol as LP
import DSL.LogProtocol hiding (StartRun)
import qualified Data.Aeson as A
import qualified Data.Yaml as Y
import Data.Aeson.TH
import RunElementClasses
import LogTransformation.Stats
import PrettyPrintCommon
import Data.Yaml.Pretty as YP
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

-- TODO: creation relational records
-- relational records from Iteration records and use reporting service
-- to provide full report - use sql lite locally 
-- see https://www.oreilly.com/library/view/microservices-antipatterns-and/9781492042716/ch04.html


--------------------------------------------------------
----------------- Iteration Aggregation ----------------
--------------------------------------------------------

data PrintLogDisplayElement =
  FilterLog [TestFilterResult] |

  StartRun {  
        title :: RunTitle, 
        config :: A.Value, 
        runStatus :: ExecutionStatus,
        testStats :: StatusCount, 
        iterationStats :: StatusCount,
        outOfTest :: StatusCount
    } | 
  EndRun (Maybe [TestFilterResult]) |

  -- StartGroup GroupTitle |
  -- EndGroup GroupTitle |

  StartTest {  
    tstTitle :: Text,
    modAddress :: TestAddress,
    notes :: Maybe Text,
    config :: A.Value, -- test Config as Json
    status :: ExecutionStatus, -- test Config as Json
    stats :: StatusCount
  }  |
  
  -- EndTest TestAddress |
  Iteration IterationRecord |
  LineError LogTransformError
  deriving (Show, Eq)

data IterationSummary = IterationSummary {
                          modulePath :: Text,
                          itmId :: Int,
                          notes :: Maybe Text,
                          pre :: Text,
                          post:: Text,
                          status :: ExecutionStatus
                        } deriving (Eq, Show)

data IterationError = IterationError {
    phase :: IterationPhase,
    error :: LogProtocolOut
  } deriving (Eq, Show)
                          
data IterationWarning = IterationWarning {
    phase :: IterationPhase,
    warning :: LogProtocolOut
  } deriving (Eq, Show)

data ApStateInfo = SucceededInteractor ApStateJSON | 
                   FailedInteractor (FrameworkError Text)
                   deriving (Eq, Show)

data PrepStateInfo = SucceededPrepState DStateJSON |
                     SkippedPrepState ItemId |
                     FailedPrepState (FrameworkError Text)
                     deriving (Eq, Show)

data IterationRecord = IterationRecord {
  modulePath :: Text,
  itmId :: Int,
  notes :: Maybe Text,
  pre :: Text,
  post:: Text,
  outcome :: IterationOutcome,
  validation :: [CheckReport],
  otherErrors :: [IterationError],
  otherWarnings :: [IterationWarning],
  item :: Maybe A.Value,
  apState :: Maybe ApStateInfo,
  domainState :: Maybe PrepStateInfo
} deriving (Eq, Show)

data IterationAccum = IterationAccum {
  rec :: Maybe IterationRecord,
  stepInfo :: LPStep,
  filterLog :: Maybe [TestFilterResult]
} deriving Show

emptyIterationAccum :: IterationAccum
emptyIterationAccum = IterationAccum {
  rec = Nothing,
  stepInfo = emptyLPStep,
  filterLog = Nothing
}

data ProblemIterationAccum = ProblemIterationAccum {
  accum :: IterationAccum,
  skipIteration :: Bool,
  skipTest :: Bool
}

emptyProbleIterationAccum :: ProblemIterationAccum
emptyProbleIterationAccum = ProblemIterationAccum emptyIterationAccum False False

printProblemsDisplayStep :: 
  RunResults                                                            -- RunResults
  -> LineNo                                                             -- lineNo
  -> ProblemIterationAccum                                              -- accum
  -> Either DeserialisationError LogProtocolOut                           -- source                           -- parse error or FrameworkError
  -> (ProblemIterationAccum, Maybe [PrintLogDisplayElement]) -- (newAccum, err / result)
printProblemsDisplayStep runResults@(RunResults _outOfTest iterationResults) lineNo (ProblemIterationAccum itAccum skipItt skipTst) eithLp = 
  let 
    -- (IterationAccum, Maybe [PrintLogDisplayElement])
    _normalStep :: (IterationAccum, Maybe [PrintLogDisplayElement]) -- (newAccum, err / result)
    _normalStep@(nxtItAccum@(IterationAccum _nxtMRec _nxtStepInfo _nxtMFltrLg), mDisplayElement) = printLogDisplayStep runResults lineNo itAccum eithLp

    testStatus' :: TestAddress -> ExecutionStatus
    testStatus' = testStatus $ testExStatus iterationResults
    
    _skipFlags :: (Bool, Bool) -- (newAccum, err / result)
    _skipFlags@(nxtSkipItt, nxtSkipTst) =
      eitherf eithLp 
        (const (skipItt, skipTst))
        (
          \LogProtocolOut{ logInfo = lp } -> 
            case lp of
              BoundaryLog (LP.StartTest (TestDisplayInfo tstMod _ _ )) -> (False, LC.Pass == testStatus' tstMod)
              BoundaryLog (StartIteration iid _ _ _) -> (LC.Pass == executionStatus (M.findWithDefault (IterationOutcome LC.Fail OutOfIteration) iid iterationResults), skipTst)
              _ -> (skipItt, skipTst)
        )

    nxtAccum :: ProblemIterationAccum
    nxtAccum = ProblemIterationAccum {
      accum = nxtItAccum,
      skipIteration = nxtSkipItt,
      skipTest = nxtSkipTst
    }
      
  in 
    (nxtAccum, isRight eithLp && (nxtSkipItt || nxtSkipTst) ? Nothing $ mDisplayElement)


-- itrOutcome ItemId -> Maybe IterationOutcome
testStatusMap :: RunResults -> M.Map TestAddress ExecutionStatus
testStatusMap = testExStatus . iterationResults

testStatus :: M.Map TestAddress ExecutionStatus -> TestAddress -> ExecutionStatus
testStatus tstStatusMap tm = fromMaybe LC.Fail $ M.lookup tm tstStatusMap

printLogDisplayStep ::
  RunResults                                                            -- RunResults
  -> LineNo                                                             -- lineNo
  -> IterationAccum                                                    -- accum
  -> Either DeserialisationError LogProtocolOut                           -- source                           -- parse error or FrameworkError
  -> (IterationAccum, Maybe [PrintLogDisplayElement]) -- (newAccum, err / result)
printLogDisplayStep runResults lineNo oldAccum@(IterationAccum{ stepInfo }) eithLp = 
  
  eitherf eithLp
   (\err -> 
      let 
        nxtFailStage = calcNextIterationFailStage (faileStage stepInfo) LC.Fail (LC.phase stepInfo) Nothing
        nxtStepInfo = stepInfo {faileStage = nxtFailStage}
      in
        (
          oldAccum { stepInfo = nxtStepInfo} :: IterationAccum, 
          Just [LineError $ LogDeserialisationError err]
        )
   )

   (\lpo@LogProtocolOut{ logInfo = lp} ->
      let 
        skipLog = (oldAccum, Nothing)

        RunResults outOfTest iterationResults = runResults

        testStatuses :: M.Map TestAddress ExecutionStatus
        testStatuses = testStatusMap runResults

        testStatus' :: TestAddress -> ExecutionStatus
        testStatus' = testStatus testStatuses

        tstIterationStatusCounts :: M.Map TestAddress StatusCount
        tstIterationStatusCounts = testIterationStatusCounts runResults

        testItrStats :: TestAddress -> StatusCount
        testItrStats tm = M.findWithDefault M.empty tm tstIterationStatusCounts

        elOut :: a -> Maybe [a]
        elOut a = Just [a]

        nxtStepInfo@(LPStep _nxtPhaseValid _nxtFailStage nxtPhase
                      _logItemStatus _nxtActiveItr _nxtCheckEncountered) = logProtocolStep stepInfo lpo

        accum :: IterationAccum
        accum = oldAccum {stepInfo = nxtStepInfo}

        lineError :: Text -> Maybe [PrintLogDisplayElement]
        lineError txt' = Just [LineError $ LogTransformError lineNo lpo txt']

        getNotes :: Y.Value -> Maybe Text
        getNotes = 
          let 
            txtOf :: Y.Value -> Maybe Text
            txtOf = 
              \case
                Y.Object _ -> Nothing
                Y.Array _ -> Nothing
                Y.String txt' -> Just txt'
                Y.Number _ -> Nothing 
                Y.Bool _ -> Nothing 
                Y.Null -> Nothing 
          in
            \case
              Y.Object obj -> firstJust [HM.lookup "notes" obj, HM.lookup "note" obj] >>= txtOf
              Y.Array _ -> Nothing
              Y.String _ -> Nothing
              Y.Number _ -> Nothing 
              Y.Bool _ -> Nothing 
              Y.Null -> Nothing 

        updateItrRec :: (IterationRecord -> IterationRecord) -> (IterationAccum, Maybe [PrintLogDisplayElement]) 
        updateItrRec func = 
          let 
            mIrec :: Maybe IterationRecord
            mIrec = rec accum
          in 
            maybef mIrec
              (accum, lineError "An iteration event has been encounterred before the start iteration event - possible loss of log event - check raw logs")
              (\irec -> (accum { rec = Just $ func irec }, Nothing))
              
        in
          case lp of
            BoundaryLog bl-> 
              case bl of
                LP.FilterLog flgs -> (accum {filterLog = Just flgs}, Nothing) 

                LP.StartRun runTitle _offset jsonCfg -> (accum, elOut $ StartRun {  
                  title = runTitle, 
                  config = jsonCfg, 
                  runStatus = worstStatus runResults,
                  testStats = testStatusCounts runResults, 
                  iterationStats = iterationStatusCounts runResults,
                  outOfTest = outOfTest
                } )
                LP.EndRun -> (accum, elOut . LogTransformation.PrintLogDisplayElement.EndRun $ filterLog accum)
                
                LP.StartGroup _ -> skipLog
                LP.EndGroup _ -> skipLog
            
                LP.StartTest (TestDisplayInfo testModAddress testTitle testConfig) -> (
                      accum, elOut $ 
                              LogTransformation.PrintLogDisplayElement.StartTest 
                                testTitle
                                testModAddress
                                (getNotes testConfig)
                                testConfig
                                (testStatus' testModAddress)
                                (testItrStats testModAddress)
                  )
                LP.EndTest _ -> skipLog
            
                StartIteration iid@(ItemId tstModule itmId) (WhenClause whn) (ThenClause thn) jsonItmVal ->  
                  (accum 
                    {rec = Just $ IterationRecord {
                      modulePath = unTestAddress tstModule,
                      itmId = itmId,
                      notes = getNotes jsonItmVal,
                      pre = whn,
                      post = thn,
                      outcome = M.findWithDefault (IterationOutcome LC.Fail OutOfIteration) iid iterationResults,
                      validation = P.empty,
                      otherErrors = P.empty,
                      otherWarnings = P.empty,
                      item = Just jsonItmVal,
                      apState = Nothing,
                      domainState = Nothing
                    } 
                  }, Nothing)

                EndIteration _ -> 
                  let 
                    severityDesc :: CheckReport -> CheckReport -> Ordering
                    severityDesc (CheckReport rslt1 _) (CheckReport rslt2 _) = compare (Down rslt1) (Down rslt2)

                    sortChecks :: IterationRecord -> IterationRecord
                    sortChecks ir = ir { validation = sortBy severityDesc $ validation ir}
                  in
                    (
                      accum {rec = Nothing}, 
                      elOut $ 
                        maybef (rec accum)
                          (LineError $ LogTransformError {
                            linNo = lineNo,
                            logItem = lpo,
                            info = "Error end iteration message encountered when the before start iteration - check raw logs"
                          })
                          (Iteration . sortChecks)
                    ) 

            IterationLog subProtocol -> 
              case subProtocol of
                Doc _ -> (accum, lineError "Documentation log item encounterred in live test log - this should not happen - probably a bug in the test runner")
                Run action -> 
                  case action of
                    IOAction _txt' -> skipLog
                    StartPrepState -> skipLog
                    StartInteraction -> skipLog
                    InteractorSuccess _iid apStateJSON -> updateItrRec (\ir -> ir {apState = Just $ SucceededInteractor apStateJSON})
                    InteractorFailure _iid err -> updateItrRec (\ir -> ir {apState = Just $ FailedInteractor err})
                  
                    PrepStateSuccess _iid dStateJSON -> updateItrRec (\ir -> ir {domainState = Just $ SucceededPrepState dStateJSON})
                    PrepStateSkipped iid -> updateItrRec (\ir -> ir {domainState = Just $ SkippedPrepState iid})
                    PrepStateFailure _iid err -> updateItrRec (\ir -> ir {domainState = Just $ FailedPrepState err})
                    StartChecks -> skipLog 
                    CheckOutcome _itmId chkReport -> updateItrRec (\ir -> ir {validation = chkReport : validation ir})
    
                    Message _ -> skipLog
                    Message' _ -> skipLog
                  
                    LP.Warning _ -> updateItrRec (\ir -> ir {otherWarnings = IterationWarning nxtPhase lpo : otherWarnings ir})
                    Warning' _ -> updateItrRec (\ir -> ir {otherWarnings = IterationWarning nxtPhase lpo : otherWarnings ir})
    
                    LP.Error _ -> updateItrRec (\ir -> ir {otherErrors = IterationError nxtPhase lpo : otherErrors ir})
      )

prettyPrintDisplayElement :: PrintLogDisplayElement -> Text
prettyPrintDisplayElement pde = 
  let 
    noImp = ""
    newLn2 = newLn <> newLn
    header' ttlTxt status = ttlTxt <> " - " <> toTitle (statusLabel False status) <> " - 00:00:88"
  in 
    case pde of
      LogTransformation.PrintLogDisplayElement.FilterLog _arrFr -> noImp

      LogTransformation.PrintLogDisplayElement.StartRun (RunTitle titl) config runStatus testStats iterationStats outOfTest -> 
            majorHeader (header' titl runStatus) 
                <> newLn
                <> "toDo - start end duration raw file" 
                <> newLn2 
                <> "runConfig: " 
                <> newLn 
                <> prettyYamlKeyValues 2 LeftJustify config
                <> newLn
                <> "test stats:" 
                <> newLn
                <> alignKeyValues True 2 RightJustify (statusCountTupleText False testStats)
                <> newLn
                <> "iteration stats:" 
                <> newLn
                <> alignKeyValues True 2 RightJustify (statusCountTupleText False iterationStats)
                <> newLn
                <> "out of test issues:" 
                <> newLn
                <> alignKeyValues True 2 RightJustify (statusCountTupleText True outOfTest)

      LogTransformation.PrintLogDisplayElement.EndRun mFltrLog -> 
        majorHeader "End Run"
        <> newLn
        <> fullHeader '-' False "Filter Log"
        <> newLn
        <> maybef mFltrLog
             "  No Filter Log Available"
             (
               \fltrs -> 
                let 
                  fltrItems :: (Maybe Text -> Bool) -> [TestFilterResult]
                  fltrItems f = P.filter (f . reasonForRejection) fltrs

                  acceptedItems:: [TestFilterResult]
                  acceptedItems = fltrItems isNothing

                  rejectedItems :: [TestFilterResult]
                  rejectedItems = fltrItems isJust

                  address :: TestFilterResult -> Text
                  address = unTestAddress . testModAddress . testInfo 

                  rejectText :: TestFilterResult -> Text
                  rejectText fr = maybef (reasonForRejection fr) 
                                    "ACCEPTED" -- should never happen
                                    (\rtxt -> address fr <> " - " <> rtxt)

                  accepted :: Text
                  accepted = P.null acceptedItems 
                              ? "  No Tests Accepted" 
                              $ indent2 . P.unlines . P.sort $ ("- " <>) . address <$> acceptedItems

                  rejected :: Text
                  rejected = P.null rejectedItems 
                              ? "  No Tests Rejected" 
                              $ indent2 . P.unlines . P.sort $ ("- " <>) . rejectText <$> rejectedItems
                in
                 "accepted: " 
                    <> newLn 
                    <> accepted 
                    <> newLn2
                    <> "rejected:"
                    <> newLn 
                    <> rejected
             )
        <> newLn


      LogTransformation.PrintLogDisplayElement.StartTest titl tstMod _notes _cfg status itrStats -> 
          majorHeader (header' titl status) 
           <> newLn
           <> "module:" 
           <> newLn
           <> indent2 (unTestAddress tstMod)
           <> newLn2 
           <> "stats:" 
           <> newLn
           <> alignKeyValues True 2 RightJustify (statusCountTupleText False itrStats)

      LogTransformation.PrintLogDisplayElement.Iteration (
          IterationRecord
            modulePath
            itmId
            notes
            when'
            then'
            (IterationOutcome status phse)
            validation
            _otherErrors
            _otherWarnings
            item
            apStateInfo
            domainState) -> 
              let 
                
                hdrLines :: [(Text, Text)]
                hdrLines = 
                  let baseLines = [
                       ("when", when')
                       , ("then", then')
                       , ("status", statusLabel False status <> (status == LC.Pass ? "" $ " - " <> txt phse))
                       ]
                  in 
                    maybef notes 
                      baseLines
                      (\n -> P.snoc baseLines ("notes:", n))

                keyOrdering :: Text -> Text -> Ordering
                keyOrdering k1 k2 = 
                  let 

                    pos :: Text -> Int
                    pos k 
                      | "iid" == k = 0
                      | "pre" == k = 1
                      | "post" == k = 2
                      | "notes" == k = 3
                      | "checks" == k = 100
                      | otherwise = 10
                  in
                    compare (pos k1) (pos k2)

                valLine :: CheckReport -> (Text, Text)
                valLine (CheckReport result MessageInfo {message} ) = (message, toLower $ txt result)

                valDetailsTxt :: Text
                valDetailsTxt = 
                  let 
                    detailValLine :: CheckReport -> Text
                    detailValLine (CheckReport result (MessageInfo hder extrInfo)) =
                        hder <> ": " <> 
                          toLower ( txt result <> 
                            maybef extrInfo 
                              " - no additional info "
                              (\exInfo -> newLn <> indentText 2 exInfo)
                        )

                    vdStep :: ([Text], Bool) -> CheckReport -> ([Text], Bool) 
                    vdStep (rsltLines, separatorNeeded) ckRpt@(CheckReport _result (MessageInfo _hder extrInfo)) = 
                      ( 
                        P.snoc rsltLines $ (separatorNeeded ? newLn $ "") <> detailValLine ckRpt, 
                        isJust extrInfo
                      )
                  in 
                   P.unlines . fst $ P.foldl' vdStep ([], False) validation
                   

                dsText :: Text
                dsText = maybef domainState
                          ("  Domain State is Empty" <> newLn)
                          (
                            \case 
                              SucceededPrepState dsDisplay -> prettyYamlKeyValues 2 LeftJustify $ unDStateJSON dsDisplay
                              SkippedPrepState _iid -> "  Domain State is Empty - Execution Skipped" <> newLn
                              FailedPrepState err -> 
                                indent2 (
                                  "PrepState Failure - Domain State is Empty:" 
                                  <> newLn 
                                  <> indent2 ("- " <> txtPretty err)
                                )
                                <> newLn
                          )

                displayApState :: ApStateInfo -> Text
                displayApState = 
                  let 
                    mkTxt :: Y.Value -> Text
                    mkTxt = indent2 . getLenient . convertString . encodePretty defConfig
                  in
                    \case 
                      SucceededInteractor val -> mkTxt $ unApStateJSON val
                      FailedInteractor err -> mkTxt $ Y.toJSON err

              in
                iterationHeader (header' (modulePath <> " - " <> txt itmId) status)
                <> newLn
                <> alignKeyValues True 0 LeftJustify hdrLines
                <> newLn
                <> "validation:"
                <> newLn
                <> (P.null validation ? "  - Test Design Error - This test has no checks\n" $ alignKeyValues True 2 LeftJustify (valLine <$> validation))
                <> newLn
                <> "domain state:"
                <> newLn
                <> dsText
 
                <> (
                P.null validation 
                   ? newLn 
                   $ newLn 
                   <> "validation details:"
                   <> newLn
                   <> indent2 valDetailsTxt
                   <> newLn2
                )
                               
                <> "application state:"
                <> newLn
                <> maybef apStateInfo "  No ApState Recorded" displayApState
                <> newLn2

                <> "full item:"
                <> newLn
                <> maybef item "  No Item Recorded" (indent2 . getLenient . convertString . encodePretty (setConfCompare keyOrdering defConfig))
                <> newLn

      LineError err -> fullHeader '!' True "ERROR"
                       <> newLn
                       <> indent2 (txt err)

statusLabel :: Bool -> ExecutionStatus -> Text
statusLabel outOfTest = \case
                          LC.Pass -> "pass"
                          LC.Fail -> outOfTest ? "error" $ "fail"
                          LC.Regression -> "regression"
                          Type2Error -> "type 2 error"
                          LC.Warning -> "warning"
                          KnownError -> "known error"

statusCountTupleText :: Bool -> StatusCount -> [(Text, Text)]
statusCountTupleText outOfTest sc = 
  let
    chkOnlyStatuses :: [ExecutionStatus]
    chkOnlyStatuses = [KnownError, Type2Error, LC.Regression, LC.Pass]

    defList :: [ExecutionStatus]
    defList = outOfTest ? enumList \\ chkOnlyStatuses $ enumList
    
    defaults :: StatusCount
    defaults = M.fromList $ (, 0) <$> defList

    displayOrder :: (ExecutionStatus, a) -> (ExecutionStatus, a) -> Ordering
    displayOrder (es1, _) (es2, _) =
      let 
        exOrd :: ExecutionStatus -> Int
        exOrd  = \case
                    LC.Pass -> 0
                    LC.Fail -> 1
                    LC.Regression -> 2
                    Type2Error -> 3
                    KnownError -> 4
                    LC.Warning -> 5
      in 
        compare (exOrd es1) (exOrd es2)

    statusTuples :: [(Text, Text)]
    statusTuples = bimap (statusLabel outOfTest) txt <$> sortBy displayOrder (M.toList (M.union sc defaults))
  in
    outOfTest 
        ? statusTuples 
        $ ("total", txt . sum $ M.elems sc) : statusTuples

$(deriveJSON defaultOptions ''PrintLogDisplayElement)
$(deriveJSON defaultOptions ''IterationRecord)
$(deriveJSON defaultOptions ''ApStateInfo)
$(deriveJSON defaultOptions ''IterationWarning)
$(deriveJSON defaultOptions ''IterationError)
$(deriveJSON defaultOptions ''PrepStateInfo)