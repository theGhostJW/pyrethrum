module LogTransformation.Stats where

import qualified Data.Map.Strict as M
import RunElementClasses as RC
import DSL.LogProtocol
import LogTransformation.Common
import Data.Aeson.TH


data StatsAccum = StatsAccum {
    runResults :: RunResults,
    stepInfo :: LPStep
  } deriving Show


emptyStatsAccum = StatsAccum {
  runResults = RunResults M.empty M.empty,
  stepInfo = emptyLPStep
}

statsStepForReducer :: LineNo                                       -- lineNo
                    -> StatsAccum                                   -- accum
                    -> Either DeserialisationError LogProtocolOut   -- Logprotocol
                    -> (StatsAccum, Maybe [StatsAccum])             -- (newAccum, err / result)
statsStepForReducer _ accum lp = (statsStep accum lp, Nothing)

statsStepFromLogProtocol :: StatsAccum -> LogProtocolOut -> StatsAccum
statsStepFromLogProtocol (StatsAccum (RunResults outOfTest itrRslts) stepInfo) lpo@LogProtocolOut{ logInfo = lp } =
  let
    nxtStepInfo@(LPStep _nxtPhaseValid _nxtFailStage nxtPhase
                    logItemStatus nxtActiveItr nxtCheckEncountered) = logProtocolStep stepInfo lpo

    inIteration :: Bool
    inIteration = isJust nxtActiveItr

    nxtOutOfTest :: StatusCount
    nxtOutOfTest = inIteration || logItemStatus == Pass --only count out of iteration messages that have not passed 
                                  ? outOfTest
                                  $ M.insertWith (+) logItemStatus 1 outOfTest

    nxtItrRslts :: IterationResults
    nxtItrRslts =
        nxtActiveItr & maybe
            itrRslts
            (\(iid, _) ->
              let
                missingChecksOutcome = IterationOutcome (
                                            isEndIteration lp && not nxtCheckEncountered
                                            ? Fail
                                            $  Pass
                                      ) Checks
                normalOutcome = IterationOutcome logItemStatus nxtPhase
              in
                M.insertWith max iid (max normalOutcome missingChecksOutcome) itrRslts
            )

  in
    StatsAccum {
      runResults = RunResults {
        outOfTest = nxtOutOfTest,
        iterationResults = nxtItrRslts
      },
      stepInfo = nxtStepInfo
    }

statsStepFromDeserialisationError :: StatsAccum -> DeserialisationError -> StatsAccum
statsStepFromDeserialisationError statsAccum@(StatsAccum (RunResults outOfTest itrRslts) stepInfo) _lp =
  let
    activeItr = stepInfo.activeIteration

    nxtOutOfTest :: StatusCount
    nxtOutOfTest = isJust activeItr
                         ?  outOfTest
                         $  M.insertWith (+) Fail 1 outOfTest

    nxtItrRslts :: IterationResults
    nxtItrRslts = activeItr & maybe
                    itrRslts
                    (\(iid, _) -> M.insertWith max iid (IterationOutcome Fail stepInfo.phase) itrRslts)
  in
    statsAccum {
      runResults = RunResults {
        outOfTest = nxtOutOfTest,
        iterationResults = nxtItrRslts
      }
    }


statsStep :: StatsAccum -> Either DeserialisationError LogProtocolOut -> StatsAccum
statsStep statsAccum = either
      (statsStepFromDeserialisationError statsAccum)
      (statsStepFromLogProtocol statsAccum)

testExStatus :: IterationResults -> M.Map Address ExecutionStatus
testExStatus ir = (.executionStatus) <$> M.mapKeysWith max (.address) ir


listTestStatus :: RunResults -> M.Map Address ExecutionStatus
listTestStatus = testExStatus . (.iterationResults)

testStatusCounts :: RunResults -> StatusCount
testStatusCounts = countValues . listTestStatus

listIterationStatus :: RunResults -> M.Map ItemId ExecutionStatus
listIterationStatus runResults = (.executionStatus) <$> runResults.iterationResults

itrStatusesGroupedByTest :: RunResults -> M.Map Address (M.Map ItemId ExecutionStatus)
itrStatusesGroupedByTest rr =
  let
    step :: M.Map Address (M.Map ItemId ExecutionStatus) -> ItemId -> ExecutionStatus -> M.Map Address (M.Map ItemId ExecutionStatus)
    step accum iid@ItemId {address} status =
       let
         tstMap :: M.Map ItemId ExecutionStatus
         tstMap = M.findWithDefault M.empty address accum
       in
        M.insert address (M.insert iid status tstMap) accum
  in
    M.foldlWithKey' step M.empty $ listIterationStatus rr

testIterationStatusCounts :: RunResults -> M.Map Address StatusCount
testIterationStatusCounts rr = countValues <$> itrStatusesGroupedByTest rr

iterationStatusCounts :: RunResults -> StatusCount
iterationStatusCounts = countValues . listIterationStatus

worstStatus :: RunResults -> ExecutionStatus
worstStatus rr@(RunResults outOfTest _) =
  let
    nonZero :: StatusCount -> [ExecutionStatus]
    nonZero = M.keys . M.filter (> 0) -- should not be in map anyway

    testStatuses :: [ExecutionStatus]
    testStatuses = nonZero $ testStatusCounts rr
  in
    null testStatuses
      ? Fail -- empty test statuses is deemed fail
      $ fromMaybe Fail $ maximum $ nonZero outOfTest <> testStatuses

$(deriveJSON defaultOptions ''StatsAccum)