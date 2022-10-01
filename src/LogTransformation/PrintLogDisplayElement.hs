module LogTransformation.PrintLogDisplayElement
  ( emptyIterationAccum,
    emptyProbleIterationAccum,
    printLogDisplayStep,
    printProblemsDisplayStep,
    prettyPrintDisplayElement,
    IterationAccum (..),
    PrintLogDisplayElement (..),
    IterationRecord (..),
    LogTransformError (..),
    IterationError (..),
    IterationWarning (..),
    ApStateInfo (..),
    ParserStatus (..),
  )
where

import Check as CK
import Common as C (DetailedInfo (..), FrameworkError (..), indentText)
import DSL.LogProtocol
import qualified DSL.LogProtocol as LP
import qualified Data.Aeson as A
import Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Yaml as Y
import Data.Yaml.Pretty as YP
import LogTransformation.Common as LC
import LogTransformation.Stats
import PrettyPrintCommon
import Pyrelude as P
  ( Bifunctor (bimap),
    Bool (..),
    Category ((.)),
    ConvertString (convertString),
    Down (Down),
    Either,
    Eq ((==)),
    Foldable (sum),
    Int,
    Lenient (getLenient),
    ListLike (empty, filter, foldl', null, snoc),
    Maybe (..),
    Monad ((>>=)),
    Ord (compare),
    Ordering,
    Semigroup ((<>)),
    Show,
    Text,
    const,
    eitherf,
    enumList,
    firstJust,
    fromMaybe,
    fst,
    isJust,
    isNothing,
    isRight,
    maybef,
    not,
    otherwise,
    sort,
    sortBy,
    toLower,
    toTitle,
    txt,
    txtPretty,
    unlines,
    ($),
    (&&),
    (<$>),
    (?),
    (\\),
    (||),
  )
import RunElementClasses

-- TODO: creation relational records
-- relational records from Iteration records and use reporting service
-- to provide full report - use sql lite locally
-- see https://www.oreilly.com/library/view/microservices-antipatterns-and/9781492042716/ch04.html

--------------------------------------------------------
----------------- Iteration Aggregation ----------------
--------------------------------------------------------

data IterationSummary = IterationSummary
  { modulePath :: Text,
    itmId :: Int,
    notes :: Maybe Text,
    pre :: Text,
    post :: Text,
    status :: ExecutionStatus
  }
  deriving (Eq, Show)

data IterationError = IterationError
  { phase :: IterationPhase,
    error :: LogProtocolOut
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''IterationError)

data IterationWarning = IterationWarning
  { phase :: IterationPhase,
    warning :: LogProtocolOut
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''IterationWarning)

data ApStateInfo
  = SucceededInteractor ApStateJSON
  | FailedInteractor (FrameworkError Text)
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ApStateInfo)

data ParserStatus
  = ParserSuccess DStateJSON
  | ParserSkipped ItemId
  | ParserFailed (FrameworkError Text)
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ParserStatus)

data IterationRecord = IterationRecord
  { address :: Address,
    itmId :: Int,
    title :: Text,
    notes :: Maybe Text,
    outcome :: IterationOutcome,
    validation :: [CheckReport],
    otherErrors :: [IterationError],
    otherWarnings :: [IterationWarning],
    item :: Maybe A.Value,
    apState :: Maybe ApStateInfo,
    domainState :: Maybe ParserStatus
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''IterationRecord)

data IterationAccum = IterationAccum
  { rec :: Maybe IterationRecord,
    stepInfo :: LPStep,
    filterLog :: Maybe [TestFilterResult]
  }
  deriving (Show)

emptyIterationAccum :: IterationAccum
emptyIterationAccum =
  IterationAccum
    { rec = Nothing,
      stepInfo = emptyLPStep,
      filterLog = Nothing
    }

data ProblemIterationAccum = ProblemIterationAccum
  { accum :: IterationAccum,
    skipIteration :: Bool,
    skipTest :: Bool
  }

emptyProbleIterationAccum :: ProblemIterationAccum
emptyProbleIterationAccum = ProblemIterationAccum emptyIterationAccum False False

data PrintLogDisplayElement
  = FilterLog [TestFilterResult]
  | StartRun
      { title :: RunTitle,
        config :: A.Value,
        runStatus :: ExecutionStatus,
        testStats :: StatusCount,
        iterationStats :: StatusCount,
        outOfTest :: StatusCount
      }
  | EndRun (Maybe [TestFilterResult])
  | -- StaXTGroup GroupTitle |
    -- EndGroup GroupTitle |

    StartTest
      { tstTitle :: Text,
        address :: Address,
        notes :: Maybe Text,
        config :: A.Value, -- test Config as Json
        status :: ExecutionStatus, -- test Config as Json
        stats :: StatusCount
      }
  | -- EndTest TestAddress |
    Iteration IterationRecord
  | LineError LogTransformError
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''PrintLogDisplayElement)

printProblemsDisplayStep ::
  RunResults -> -- RunResults
  LineNo -> -- lineNo
  ProblemIterationAccum -> -- accum
  Either DeserialisationError LogProtocolOut -> -- source                           -- parse error or FrameworkError
  (ProblemIterationAccum, Maybe [PrintLogDisplayElement]) -- (newAccum, err / result)
printProblemsDisplayStep runResults@(RunResults _outOfTest iterationResults) lineNo (ProblemIterationAccum itAccum skipItt skipTst) eithLp =
  let -- (IterationAccum, Maybe [PrintLogDisplayElement])
      _normalStep :: (IterationAccum, Maybe [PrintLogDisplayElement]) -- (newAccum, err / result)
      _normalStep@(nxtItAccum@(IterationAccum _nxtMRec _nxtStepInfo _nxtMFltrLg), mDisplayElement) = printLogDisplayStep runResults lineNo itAccum eithLp

      testStatus' :: Address -> ExecutionStatus
      testStatus' = testStatus $ testExStatus iterationResults

      _skipFlags :: (Bool, Bool) -- (newAccum, err / result)
      _skipFlags@(nxtSkipItt, nxtSkipTst) =
        eitherf
          eithLp
          (const (skipItt, skipTst))
          ( \LogProtocolOut {logInfo = lp} ->
              case lp of
                LP.StartTest (TestLogInfo ttl domain _) -> (False, LC.Pass == testStatus' (push ttl Test domain))
                StartIteration iid _ _ -> (LC.Pass == executionStatus (M.findWithDefault (IterationOutcome LC.Fail OutOfIteration) iid iterationResults), skipTst)
                _ -> (skipItt, skipTst)
          )

      nxtAccum :: ProblemIterationAccum
      nxtAccum =
        ProblemIterationAccum
          { accum = nxtItAccum,
            skipIteration = nxtSkipItt,
            skipTest = nxtSkipTst
          }
   in (nxtAccum, isRight eithLp && (nxtSkipItt || nxtSkipTst) ? Nothing $ mDisplayElement)

-- itrOutcome ItemId -> Maybe IterationOutcome
testStatusMap :: RunResults -> M.Map Address ExecutionStatus
testStatusMap = testExStatus . iterationResults

testStatus :: M.Map Address ExecutionStatus -> Address -> ExecutionStatus
testStatus tstStatusMap tm = fromMaybe LC.Fail $ M.lookup tm tstStatusMap

printLogDisplay ::
  RunResults -> -- RunResults
  LineNo -> -- lineNo
  IterationAccum -> -- accum
  LogProtocolOut -> -- source   -- parse error or FrameworkError
  (IterationAccum, Maybe [PrintLogDisplayElement]) -- (newAccum, err / result)
printLogDisplay runResults lineNo oldAccum@IterationAccum {stepInfo} lpo@LogProtocolOut {logInfo = lp} =
  let skipLog = (oldAccum, Nothing)

      RunResults outOfTest iterationResults = runResults

      testStatuses :: M.Map Address ExecutionStatus
      testStatuses = testStatusMap runResults

      testStatus' :: Address -> ExecutionStatus
      testStatus' = testStatus testStatuses

      tstIterationStatusCounts :: M.Map Address StatusCount
      tstIterationStatusCounts = testIterationStatusCounts runResults

      testItrStats :: Address -> StatusCount
      testItrStats tm = M.findWithDefault M.empty tm tstIterationStatusCounts

      elOut :: a -> Maybe [a]
      elOut a = Just [a]

      nxtStepInfo :: LPStep
      nxtStepInfo@( LPStep
                      _nxtPhaseValid
                      _nxtFailStage
                      nxtPhase
                      _logItemStatus
                      _nxtActiveItr
                      _nxtCheckEncountered
                    ) = logProtocolStep stepInfo lpo

      accum :: IterationAccum
      accum = oldAccum {stepInfo = nxtStepInfo}

      lineError :: Text -> Maybe [PrintLogDisplayElement]
      lineError txt' = Just [LineError $ LogTransformError lineNo lpo txt']

      txtOf :: Y.Value -> Maybe Text
      txtOf =
        \case
          Y.Object _ -> Nothing
          Y.Array _ -> Nothing
          Y.String txt' -> Just txt'
          Y.Number _ -> Nothing
          Y.Bool _ -> Nothing
          Y.Null -> Nothing

      getNotes :: Y.Value -> Maybe Text
      getNotes =
        \case
          Y.Object obj -> firstJust [HM.lookup "notes" obj, HM.lookup "note" obj] >>= txtOf
          Y.Array _ -> Nothing
          Y.String _ -> Nothing
          Y.Number _ -> Nothing
          Y.Bool _ -> Nothing
          Y.Null -> Nothing

      updateItrRec :: (IterationRecord -> IterationRecord) -> (IterationAccum, Maybe [PrintLogDisplayElement])
      updateItrRec func =
        let mIrec :: Maybe IterationRecord
            mIrec = rec accum
         in maybef
              mIrec
              (accum, lineError "An iteration event has been encounterred before the start iteration event - possible loss of log event - check raw logs")
              (\irec -> (accum {rec = Just $ func irec}, Nothing))
   in case lp of
        LP.FilterLog flgs -> (accum {filterLog = Just flgs}, Nothing)
        LP.StartRun runTitle _offset jsonCfg ->
          ( accum,
            elOut $
              LogTransformation.PrintLogDisplayElement.StartRun
                { title = runTitle,
                  config = jsonCfg,
                  runStatus = worstStatus runResults,
                  testStats = testStatusCounts runResults,
                  iterationStats = iterationStatusCounts runResults,
                  outOfTest = outOfTest
                }
          )
        LP.EndRun -> (accum, elOut . LogTransformation.PrintLogDisplayElement.EndRun $ filterLog accum)
        LP.StaXTGroup _ -> skipLog
        LP.EndGroup _ -> skipLog
        StartHook {} -> skipLog
        EndHook {} -> skipLog
        LP.StartTest (TestLogInfo ttl domain testConfig) ->
          let elmAddress = push ttl Test domain
           in ( accum,
                elOut $
                  LogTransformation.PrintLogDisplayElement.StartTest
                    ttl
                    elmAddress
                    (getNotes testConfig)
                    testConfig
                    (testStatus' elmAddress)
                    (testItrStats elmAddress)
              )
        LP.EndTest _ -> skipLog
        StartIteration iid@(ItemId address itmId) ttle jsonItmVal ->
          ( accum
              { rec =
                  Just $
                    IterationRecord
                      { address = address,
                        itmId = itmId,
                        title = ttle,
                        notes = getNotes jsonItmVal,
                        outcome = M.findWithDefault (IterationOutcome LC.Fail OutOfIteration) iid iterationResults,
                        validation = P.empty,
                        otherErrors = P.empty,
                        otherWarnings = P.empty,
                        item = Just jsonItmVal,
                        apState = Nothing,
                        domainState = Nothing
                      }
              },
            Nothing
          )
        EndIteration _ ->
          let severityDesc :: CheckReport -> CheckReport -> Ordering
              severityDesc (CheckReport rslt1 _) (CheckReport rslt2 _) = compare (Down rslt1) (Down rslt2)

              sortChecks :: IterationRecord -> IterationRecord
              sortChecks ir = ir {validation = sortBy severityDesc $ validation ir}
           in ( accum {rec = Nothing},
                elOut $
                  maybef
                    (rec accum)
                    ( LineError $
                        LogTransformError
                          { linNo = lineNo,
                            logItem = lpo,
                            info = "Error end iteration message encountered when the before start iteration - check raw logs"
                          }
                    )
                    (Iteration . sortChecks)
              )
        IOAction _ -> skipLog
        IOAction' _ -> skipLog
        StartParser -> skipLog
        StartInteraction -> skipLog
        InteractorSuccess _iid apStateJSON -> updateItrRec (\ir -> ir {apState = Just $ SucceededInteractor apStateJSON})
        InteractorFailure _iid err -> updateItrRec (\ir -> ir {apState = Just $ FailedInteractor err})
        LP.ParserSuccess _iid dStateJSON -> updateItrRec (\ir -> ir {domainState = Just $ LogTransformation.PrintLogDisplayElement.ParserSuccess dStateJSON})
        LP.ParserSkipped iid -> updateItrRec (\ir -> ir {domainState = Just $ LogTransformation.PrintLogDisplayElement.ParserSkipped iid})
        ParserFailure _iid err -> updateItrRec (\ir -> ir {domainState = Just $ ParserFailed err})
        StartChecks -> skipLog
        CheckOutcome _itmId chkReport -> updateItrRec (\ir -> ir {validation = chkReport : validation ir})
        Message _ -> skipLog
        Message' _ -> skipLog
        LP.Warning _ -> updateItrRec (\ir -> ir {otherWarnings = IterationWarning nxtPhase lpo : otherWarnings ir})
        Warning' _ -> updateItrRec (\ir -> ir {otherWarnings = IterationWarning nxtPhase lpo : otherWarnings ir})
        LP.Error _ -> updateItrRec (\ir -> ir {otherErrors = IterationError nxtPhase lpo : otherErrors ir})

printLogDisplayStep ::
  RunResults -> -- RunResults
  LineNo -> -- lineNo
  IterationAccum -> -- accum
  Either DeserialisationError LogProtocolOut -> -- source   -- parse error or FrameworkError
  (IterationAccum, Maybe [PrintLogDisplayElement]) -- (newAccum, err / result)
printLogDisplayStep runResults lineNo oldAccum@IterationAccum {stepInfo} eithLp =
  eitherf
    eithLp
    ( \err ->
        let nxtFailStage = calcNextIterationFailStage (faileStage stepInfo) LC.Fail (LC.phase stepInfo) Nothing
            nxtStepInfo = stepInfo {faileStage = nxtFailStage}
         in (oldAccum {stepInfo = nxtStepInfo} :: IterationAccum, Just [LineError $ LogDeserialisationError err])
    )
    (printLogDisplay runResults lineNo oldAccum)

prettyPrintDisplayElement :: PrintLogDisplayElement -> Text
prettyPrintDisplayElement pde =
  let noImp = ""
      newLn2 = newLn <> newLn
      header' ttlTxt status = ttlTxt <> " - " <> toTitle (statusLabel False status) <> " - 00:00:88"
   in case pde of
        LogTransformation.PrintLogDisplayElement.FilterLog _arrFr -> noImp
        LogTransformation.PrintLogDisplayElement.StartRun (RunTitle titl) config runStatus testStats iterationStats outOfTest ->
          majorHeader False (header' titl runStatus)
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
          majorHeader False "End Run"
            <> newLn
            <> fullHeader False '-' False "Filter Log"
            <> newLn
            <> maybef
              mFltrLog
              "  No Filter Log Available"
              ( \fltrs ->
                  let fltrItems :: (Maybe Text -> Bool) -> [TestFilterResult]
                      fltrItems f = P.filter (f . reasonForRejection) fltrs

                      acceptedItems :: [TestFilterResult]
                      acceptedItems = fltrItems isNothing

                      rejectedItems :: [TestFilterResult]
                      rejectedItems = fltrItems isJust

                      address :: TestFilterResult -> Text
                      address = logInfoAddress . testInfo

                      rejectText :: TestFilterResult -> Text
                      rejectText fr =
                        maybef
                          (reasonForRejection fr)
                          "ACCEPTED" -- should never happen
                          (\rtxt -> address fr <> " - " <> rtxt)

                      accepted :: Text
                      accepted =
                        P.null acceptedItems
                          ? "  No Tests Accepted"
                          $ indent2 . P.unlines . P.sort $ ("- " <>) . address <$> acceptedItems

                      rejected :: Text
                      rejected =
                        P.null rejectedItems
                          ? "  No Tests Rejected"
                          $ indent2 . P.unlines . P.sort $ ("- " <>) . rejectText <$> rejectedItems
                   in "accepted: "
                        <> newLn
                        <> accepted
                        <> newLn2
                        <> "rejected:"
                        <> newLn
                        <> rejected
              )
            <> newLn
        LogTransformation.PrintLogDisplayElement.StartTest titl address _notes _cfg status itrStats ->
          majorHeader False (header' titl status)
            <> newLn
            <> "module:"
            <> newLn
            <> indent2 (render address)
            <> newLn2
            <> "stats:"
            <> newLn
            <> alignKeyValues True 2 RightJustify (statusCountTupleText False itrStats)
        LogTransformation.PrintLogDisplayElement.Iteration
          ( IterationRecord
              address
              itmId
              ttle
              notes
              (IterationOutcome status phse)
              validation
              _otherErrors
              _otherWarnings
              item
              apStateInfo
              domainState
            ) ->
            let hdrLines :: [(Text, Text)]
                hdrLines =
                  let baseLines =
                        [ ("title", ttle),
                          ("status", statusLabel False status <> (status == LC.Pass ? "" $ " - " <> txt phse))
                        ]
                   in maybef
                        notes
                        baseLines
                        (\n -> P.snoc baseLines ("notes:", n))

                keyOrdering :: Text -> Text -> Ordering
                keyOrdering k1 k2 =
                  let pos :: Text -> Int
                      pos k
                        | "iid" == k = 0
                        | "title" == k = 1
                        | "notes" == k = 2
                        | "checks" == k = 100
                        | otherwise = 10
                   in compare (pos k1) (pos k2)

                valLine :: CheckReport -> (Text, Text)
                valLine (CheckReport result DetailedInfo {message}) = (message, toLower $ txt result)

                valDetailsTxt :: Text
                valDetailsTxt =
                  let detailValLine :: CheckReport -> Text
                      detailValLine (CheckReport result (DetailedInfo hder extrInfo)) =
                        hder <> ": "
                          <> toLower
                            ( txt result
                                <> ( null extrInfo
                                       ? " - no additional info "
                                       $ newLn <> indentText 2 extrInfo
                                   )
                            )

                      vdStep :: ([Text], Bool) -> CheckReport -> ([Text], Bool)
                      vdStep (rsltLines, separatorNeeded) ckRpt@(CheckReport _result (DetailedInfo _hder extrInfo)) =
                        ( P.snoc rsltLines $ (separatorNeeded ? newLn $ "") <> detailValLine ckRpt,
                          not $ null extrInfo
                        )
                   in P.unlines . fst $ P.foldl' vdStep ([], False) validation

                dsText :: Text
                dsText =
                  maybef
                    domainState
                    ("  Domain State is Empty" <> newLn)
                    ( \case
                        LogTransformation.PrintLogDisplayElement.ParserSuccess dsDisplay -> prettyYamlKeyValues 2 LeftJustify $ unDStateJSON dsDisplay
                        LogTransformation.PrintLogDisplayElement.ParserSkipped _iid -> "  Domain State is Empty - Execution Skipped" <> newLn
                        ParserFailed err ->
                          indent2
                            ( "ParseFailure - Domain State is Empty:"
                                <> newLn
                                <> indent2 ("- " <> txtPretty err)
                            )
                            <> newLn
                    )

                displayApState :: ApStateInfo -> Text
                displayApState =
                  let mkTxt :: Y.Value -> Text
                      mkTxt = indent2 . getLenient . convertString . encodePretty defConfig
                   in \case
                        SucceededInteractor val -> mkTxt $ unApStateJSON val
                        FailedInteractor err -> mkTxt $ Y.toJSON err
             in iterationHeader False (header' (render address <> " - " <> txt itmId) status)
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
                  <> ( P.null validation
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
        LineError err ->
          fullHeader False '!' True "ERROR"
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
  let chkOnlyStatuses :: [ExecutionStatus]
      chkOnlyStatuses = [KnownError, Type2Error, LC.Regression, LC.Pass]

      defList :: [ExecutionStatus]
      defList = outOfTest ? enumList \\ chkOnlyStatuses $ enumList

      defaults :: StatusCount
      defaults = M.fromList $ (,0) <$> defList

      displayOrder :: (ExecutionStatus, a) -> (ExecutionStatus, a) -> Ordering
      displayOrder (es1, _) (es2, _) =
        let exOrd :: ExecutionStatus -> Int
            exOrd = \case
              LC.Pass -> 0
              LC.Fail -> 1
              LC.Regression -> 2
              Type2Error -> 3
              KnownError -> 4
              LC.Warning -> 5
         in compare (exOrd es1) (exOrd es2)

      statusTuples :: [(Text, Text)]
      statusTuples = bimap (statusLabel outOfTest) txt <$> sortBy displayOrder (M.toList (M.union sc defaults))
   in outOfTest
        ? statusTuples
        $ ("total", txt . sum $ M.elems sc) : statusTuples
