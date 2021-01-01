{-# LANGUAGE NoPolyKinds #-} 
-- TODO: work out why this is needed - investigate polykinds

module Runner (
  mkEndpointSem
  , RunParams(..)
  , mkRunSem
  , module RB
  , module ItemFilter
  , module C
) where

import Common as C
import DSL.Interpreter
import DSL.Logger
import DSL.LogProtocol as LP
import DSL.CurrentTime
import Pyrelude as P
import Polysemy
import Polysemy.Error as PE
import ItemFilter  (ItemFilter (..), filterredItemIds)
import qualified Data.Set as S
import RunElementClasses as C
import OrphanedInstances()
import Data.Aeson
import TestFilter
import RunnerBase as RB
import qualified Prelude

runTestItems :: forall i as ds tc rc e effs. (ToJSON e, Show e, TestConfigClass tc, ItemClass i ds, Member (Logger e) effs) =>
      Maybe (S.Set Int)                                                    -- target Ids
      -> [i]   
      -> rc                                                            -- items
      -> Test e tc rc i as ds effs
      -> ItemRunner e as ds i tc rc effs
      -> [Sem effs ()]
runTestItems iIds items rc test@Test{ config = tc } itemRunner =
  let
    logBoundry :: BoundaryEvent -> Sem effs ()
    logBoundry = logItem . BoundaryLog

    startTest :: Sem effs ()
    startTest = logBoundry . StartTest $ mkDisplayInfo tc

    endTest :: Sem effs ()
    endTest = logBoundry . EndTest $ moduleAddress tc

    filteredItems :: [i]
    filteredItems = filter inTargIds items

    applyRunner :: i -> Sem effs ()
    applyRunner i =  
      let
        iid :: ItemId
        iid = ItemId (moduleAddress tc) (identifier i)
      in
        do
          logBoundry . StartIteration iid (WhenClause $ whenClause i) (ThenClause $ thenClause i) $ toJSON i
          itemRunner rc test i
          logBoundry $ EndIteration iid

    inTargIds :: i -> Bool
    inTargIds i = maybe True (S.member (identifier i)) iIds

  in
    case filteredItems of
      [] -> []
      [x] -> [startTest *> applyRunner x *> endTest]
      x : xs -> (startTest *> applyRunner x)
                : (applyRunner <$> Prelude.init xs)
                <> [applyRunner (Prelude.last xs) *> endTest]

runTest ::  forall i rc as ds tc e effs. (ItemClass i ds, TestConfigClass tc, ToJSON e, Show e, Member (Logger e) effs) =>
                   Maybe (S.Set Int)                     -- target Ids
                   -> FilterList rc tc                   -- filters
                   -> ItemRunner e as ds i tc rc effs    -- item runner
                   -> rc                                 -- runConfig
                   -> Test e tc rc i as ds effs          -- Test Case
                   -> [Sem effs ()]                      -- [TestIterations]
runTest iIds fltrs itemRunner rc test@Test {config = tc, items} =
    acceptFilter (filterTestCfg fltrs rc tc)
    ? runTestItems
        iIds 
        (items rc) 
        rc
        test
        itemRunner
        $ []

logLPError ::  forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => FrameworkError e -> Sem effs ()
logLPError = logItem . logRun . LP.Error

runHook :: forall e effs. Member (Error (FrameworkError e)) effs => PreRun effs -> PreTestStage -> Sem effs (Either (FrameworkError e) ())
runHook PreRun{runAction, checkHasRun} stage = 
  do
    let
      stageStr :: Text
      stageStr = txt stage

      stageExLabel :: Text
      stageExLabel = "Execution of " <> stageStr

      msgPrefix :: Text
      msgPrefix = case stage of
                    Rollover -> "No tests run in group. "
                    GoHome -> "No items run for test. "

      rolloverFailError :: FrameworkError e -> FrameworkError e
      rolloverFailError = PreTestCheckExecutionError stage $ msgPrefix <> stageExLabel <> " check"

      rolloverCheckFalseError :: FrameworkError e
      rolloverCheckFalseError = PreTestCheckError stage
                                  $ msgPrefix
                                  <> stageStr
                                  <> " action ran without exception but completion check returned False. Looks like "
                                  <> stageStr
                                  <> " did not run as expected"
      exeHook :: Sem effs ()
      exeHook = 
        do 
          PE.catch runAction (PE.throw . rolloverFailError)
          runCheck <- PE.catch checkHasRun (PE.throw . PreTestCheckExecutionError stage "exception encountered verifying hook has run")
          unless runCheck (PE.throw rolloverCheckFalseError)
      
    (Right <$> exeHook) `PE.catch` (pure . Left)


data RunParams e rc tc effs = RunParams {
  plan :: forall mo mi a. TestPlanBase e tc rc mo mi a effs,
  filters :: FilterList rc tc,
  itemRunner :: forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => ItemRunner e as ds i tc rc effs,
  rc :: rc
}

mkSem :: forall rc tc e effs. (ToJSON e, Show e, RunConfigClass rc, TestConfigClass tc, ApEffs e effs) =>
                    Maybe (S.Set Int)                                   -- a set of item Ids used for test case endpoints
                    -> RunParams e rc tc effs
                    -> Sem effs ()
mkSem iIds RunParams {plan, filters, rc, itemRunner} =
  let
    filterInfo :: [[FilterResult]]
    filterInfo = filterGroups plan filters rc

    filterFlags :: [Bool]
    filterFlags = filterGroupFlags filterInfo

    prepResults :: [RunElement [] (Sem effs) () effs]
    prepResults = plan $ runTest iIds filters itemRunner rc 

    firstDuplicateGroupTitle :: Maybe Text
    firstDuplicateGroupTitle = toS <$> firstDuplicate (toS . C.title <$> prepResults :: [Prelude.String])

    runTuples ::  [(Bool, RunElement [] (Sem effs) () effs)]
    runTuples = P.zip filterFlags prepResults

    logBoundry :: BoundaryEvent -> Sem effs ()
    logBoundry = logItem . BoundaryLog

    exeGroup :: (Bool, RunElement [] (Sem effs) () effs) -> Sem effs ()
    exeGroup (include, tg) =
      let
        -- if ids are passed in we are running an endpoint
        -- endpoint go home and rolllover are not run if the application is already home
        guardedHookRun :: (RunElement [] (Sem effs) () effs -> PreRun effs) -> PreTestStage -> Sem effs (Either (FrameworkError e) ())
        guardedHookRun hookSelector hookLabel =
          do 
            wantHookRun <- isJust iIds ? 
                            (not <$> checkHasRun (goHome tg)) $ 
                            pure True
            wantHookRun ? runHook (hookSelector tg) hookLabel $ pure $ Right ()

        rollover' :: Sem effs (Either (FrameworkError e) ())
        rollover' = guardedHookRun rollover Rollover

        goHome' :: Sem effs (Either (FrameworkError e) ())
        goHome' = guardedHookRun goHome GoHome

        hookThenRun :: Sem effs (Either (FrameworkError e) ()) -> Sem effs () -> Sem effs ()
        hookThenRun hook mRun = do
                                  eth <- hook
                                  eitherf eth
                                    logLPError
                                    (const mRun)

        runTest' :: [Sem effs ()] -> Sem effs ()
        runTest' testIterations = sequence_ (hookThenRun goHome' <$> testIterations)

        runTests :: Sem effs ()
        runTests = sequence_ $ runTest' <$> filter (not . null) (tests tg)

        runGrp :: Sem effs ()
        runGrp = 
              let 
                hdr = GroupTitle $ RB.header tg
              in
                do
                  logBoundry $ StartGroup hdr
                  hookThenRun rollover' runTests
                  logBoundry $ EndGroup hdr
      in
        include ? runGrp $ pure ()
  in
    maybef firstDuplicateGroupTitle
    (
      do
        offset' <- utcOffset
        logBoundry . StartRun (RunTitle $ C.title rc) offset' $ toJSON rc
        logBoundry . FilterLog $ filterLog filterInfo
        sequence_ $ exeGroup <$> runTuples
        logBoundry EndRun
    )
    (\dupeTxt -> logLPError . C.Error $ "Test Run Configuration Error. Duplicate Group Names: " <> dupeTxt)

mkRunSem :: forall rc tc e effs. (RunConfigClass rc, TestConfigClass tc, ToJSON e, Show e, ApEffs e effs) => RunParams e rc tc effs -> Sem effs ()
mkRunSem = mkSem Nothing 

mkEndpointSem :: forall rc tc e effs. (RunConfigClass rc, TestConfigClass tc, ToJSON e, Show e, ApEffs e effs) =>
                   RunParams e rc tc effs
                   -> TestAddress                            -- test address
                   -> Either FilterErrorType (S.Set Int)    -- a set of item Ids used for test case endpoints                                               -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> Sem effs ()
mkEndpointSem runParams@RunParams { filters } tstAddress iIds =
  let
    endpointFilter :: TestAddress -> TestFilter rc tc
    endpointFilter targAddress = TestFilter {
      title = "test address does not match endpoint target: " <> toString targAddress,
      predicate = \_ tc -> moduleAddress tc == targAddress
    }

    allFilters :: [TestFilter rc tc]
    allFilters = endpointFilter tstAddress : filters
  in
    eitherf iIds
      (logItem . logRun . LP.Error . FilterError)
      (\idSet -> mkSem (Just idSet) runParams { filters = allFilters })