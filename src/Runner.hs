{-# LANGUAGE NoPolyKinds #-} 
-- TODO: work out why this is needed - investigate polykinds

module Runner (
  applyTestFilters
  , docExecution
  , doNothing
  , logFileHandles
  , normalExecution
  , showAndLogItems
  , mkEndpointSem
  , TestPlanBase
  , TestParams
  , RunParams(..)
  , ItemParams
  , mkRunSem
  , module RB
  , module ItemFilter
  , module C
) where

import qualified Check as CK
import Common as C
import DSL.Interpreter
import DSL.Logger
import DSL.LogProtocol as LP
import DSL.Ensure
import DSL.CurrentTime
import Pyrelude as P
import Pyrelude.IO
import Polysemy
import Polysemy.Error as PE
import qualified Data.DList as D
import           ItemFilter  (ItemFilter (..), filterredItemIds)
import qualified Data.Set as S
import RunElementClasses as C
import Text.Show.Pretty
import AuxFiles
import OrphanedInstances()
import Data.Aeson
import TestFilter
import RunnerBase as RB
import qualified System.IO as SIO
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import qualified Prelude

type TestPlanBase e tc rc m1 m a effs = (forall i as ds. (ItemClass i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) => 
                                                          GenericTest e tc rc i as ds effs -> m1 (m a)) -> [RunElement m1 m a effs]

--- Reapplying test Filters to Items ---

applyTestFilters :: forall i tc rc. TestConfigClass tc => [TestFilter rc tc] -> rc -> (i -> tc) -> [i] -> [i]
applyTestFilters fltrs rc cvtr itms = 
    fst <$> filter (isNothing . snd) (applyTestFiltersToItemsShowReason fltrs rc cvtr itms) 

-- de bugging
applyTestFiltersToItemsShowReason :: forall i tc rc. TestConfigClass tc => [TestFilter rc tc] -> rc -> (i -> tc) -> [i] -> [(i, Maybe Text)]
applyTestFiltersToItemsShowReason fltrs rc cvtr itms = 
  let 
    fltrItm :: i -> (i, Maybe Text)
    fltrItm i = (i, reasonForRejection . filterTestCfg fltrs rc $ cvtr i)
  in 
    fltrItm <$> itms

---

showAndLogItems :: Show a => [a] -> IO ()
showAndLogItems = showAndLogList "items"

showAndLogList :: Show a => Text -> [a] -> IO ()
showAndLogList logSuffix items = 
      let 
        logSpec :: M.Map (Text, FileExt) ()
        logSpec = M.singleton (logSuffix, FileExt ".log") ()

        hndle :: IO (Either (FrameworkError e) HandleInfo)
        hndle = either
                  Left
                  (
                    maybe
                      (Left $ C.Error "showAndLogList - no Handle returned")
                      (Right . snd)
                    . head
                  ) 
                <$> logFileHandles logSpec

        log2Both :: SIO.Handle -> Text -> IO ()
        log2Both fileHndl lgStr = putLines SIO.stdout lgStr *> putLines fileHndl lgStr

        listItems :: SIO.Handle -> IO ()
        listItems h = sequence_ $ log2Both h . txtPretty <$> items
      in
        hndle >>=
                either pPrint (\HandleInfo{path, fileHandle} -> 
                                  listItems fileHandle `finally` SIO.hClose fileHandle
                                  *> putStrLn ""
                                  *> putStrLn "--- Log Files ---"
                                  *> putStrLn (toS . toFilePath $ path)
                                  *> putStrLn ""
                              )


logFileHandles :: forall a e. M.Map (Text, FileExt) a -> IO (Either (FrameworkError e) [(a, HandleInfo)])
logFileHandles suffixExtensionMap = 
  let
    openHandle :: (Text, FileExt) -> a -> IO (Either (FrameworkError e) (a, HandleInfo))
    openHandle (suff, ext) a = 
      do 
        eHandInfo <- logFileHandle suff ext
        pure $ eitherf eHandInfo
                (Left . IOError' "Error creating log file" )
                (\hInfo -> Right (a, hInfo))

    openHandles :: IO [((Text, FileExt), Either (FrameworkError e) (a, HandleInfo))]
    openHandles = M.toList <$> M.traverseWithKey openHandle suffixExtensionMap
  in 
    do 
      hlst <- openHandles
      let 
        fstErr = find (isLeft . snd) hlst
        openHndls = rights (snd <$> hlst)
      maybef fstErr
        (pure $ Right openHndls)
        (
          \((sfx, ext), fstErr') -> 
            do 
              traverse_ (hClose . fileHandle . snd) openHndls
              pure . Left $ AnnotatedError 
                              ("Failed to create log file with suffix: " <> sfx) 
                              $ fromLeft (C.Error "won't happen") fstErr'
        )

doNothing :: PreRun effs
doNothing = PreRun {
  runAction = pure (),
  checkHasRun = pure True
}

disablePreRun :: RunElement m m1 a effs -> RunElement m m1 a effs
disablePreRun tg = tg {
                        rollover = doNothing,
                        goHome = doNothing
                      }

testAddress :: forall tc rc i effs as ds e. TestConfigClass tc => GenericTest e tc rc i effs as ds -> TestModule
testAddress =  moduleAddress . (config :: GenericTest e tc rc i effs as ds -> tc)


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Run Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                   

data TestParams e as ds i tc rc effs = TestParams {                       
  interactor :: rc -> i -> Sem effs as,                         
  prepState :: forall pEffs. (Ensurable e) pEffs => i -> as -> Sem pEffs ds,                      
  tc :: tc,                                                     
  rc :: rc                                                      
}

data ItemParams e as ds i tc rc effs = ItemParams {
  testParams :: TestParams e as ds i tc rc effs,                                                     
  item :: i                                                        
}

logLP :: forall e effs. (Show e, ToJSON e, Member (Logger e) effs) => LogProtocolBase e -> Sem effs ()
logLP = logItem 

logRP :: forall e effs. (Show e, ToJSON e, Member (Logger e) effs) => RunProtocol e -> Sem effs ()
logRP = logLP . logRun 

normalExecution :: forall e effs rc tc i as ds. (ItemClass i ds, ToJSON as, ToJSON ds, TestConfigClass tc, ToJSON e, Show e, ApEffs e effs) 
                      => ItemParams e as ds i tc rc effs -> Sem effs ()  
normalExecution (ItemParams (TestParams interactor prepState tc rc) i)  = 
  let
    iid :: ItemId
    iid = ItemId (moduleAddress tc) (identifier i)

    logChk :: CK.CheckReport -> Sem effs ()
    logChk cr = logRP $ CheckOutcome iid cr

    recordSkippedChecks :: Sem effs ()
    recordSkippedChecks = do 
                            logRP StartChecks 
                            F.traverse_ logChk $ D.toList $ CK.skipChecks (checkList i)

    prepStateErrorHandler :: FrameworkError e -> Sem effs ds
    prepStateErrorHandler e = 
      do 
        logRP $ PrepStateFailure iid e
        recordSkippedChecks
        PE.throw e

    -- provided natively by polysemy in later versions of polysemy
    try' :: Member (Error er) r => Sem r a -> Sem r (Either er a)
    try' m = PE.catch (Right <$> m) (return . Left)
       
    normalExecution' :: Sem effs ()
    normalExecution' = 
      let
        runChecks :: ds -> Sem effs ()
        runChecks ds = F.traverse_ logChk $ D.toList $ CK.calcChecks ds (checkList i)
      in 
        do 
          logRP StartInteraction
          -- TODO: check for io exceptions / SomeException - use throw from test
          log "interact start"
          ethApState <- try' $ interactor rc i
          eitherf ethApState
            (\e -> do 
                    logRP $ InteractorFailure iid e
                    logRP $ PrepStateSkipped iid
                    recordSkippedChecks
                    )
            (\as -> do 
                log "interact end"
                logRP . InteractorSuccess iid . ApStateJSON . toJSON $ as
                logRP StartPrepState
                ds <- PE.catch (prepState i as) prepStateErrorHandler
                logRP . PrepStateSuccess iid . DStateJSON . toJSON $ ds
                logRP StartChecks
                runChecks ds
              )
  in 
    normalExecution' `PE.catch` (logRP . LP.Error)

docExecution :: forall e effs rc tc i as ds. (ToJSON e, Show e, ItemClass i ds, TestConfigClass tc, Member (Logger e) effs)
              => ItemParams e as ds i tc rc effs -> Sem effs () 
docExecution (ItemParams (TestParams interactor prepState tc rc) i) = 
  let
    iid :: ItemId
    iid = ItemId (moduleAddress tc) $ identifier i

    docLog :: DocProtocol e -> Sem effs ()
    docLog = logLP . logDoc

    logChecks :: Sem effs ()
    logChecks =  P.sequence_ $  
                    (\chk -> docLog $ DocCheck iid (CK.header (chk :: CK.Check ds)) (CK.expectation chk) (CK.gateStatus chk)) <$> D.toList (checkList i)
  in 
    do 
      docLog DocInteraction
      interactor rc i
      docLog DocChecks
      logChecks

runTestItems :: forall i as ds tc rc e effs. (ToJSON e, Show e, TestConfigClass tc, ItemClass i ds, Member (Logger e) effs) =>
      Maybe (S.Set Int)                                                    -- target Ids
      -> [i]                                                               -- items
      -> TestParams e as ds i tc rc effs
      -> (ItemParams e as ds i tc rc effs -> Sem effs ())
      -> [Sem effs ()]
runTestItems iIds items runPrms@(TestParams interactor prepState tc rc) itemRunner =
  let
    logBoundry :: BoundaryEvent -> Sem effs ()
    logBoundry = logLP . BoundaryLog

    startTest :: Sem effs ()
    startTest = logBoundry . StartTest $ mkDisplayInfo tc

    endTest :: Sem effs ()
    endTest = logBoundry . EndTest $ moduleAddress tc

    filteredItems :: [i]
    filteredItems = filter inTargIds items

    runItem :: i -> Sem effs ()
    runItem i =  let
                    iid :: ItemId
                    iid = ItemId (moduleAddress tc) (identifier i)
                  in
                    do
                      logBoundry . StartIteration iid (WhenClause $ whenClause i) (ThenClause $ thenClause i) $ toJSON i
                      itemRunner $ ItemParams runPrms i
                      logBoundry $ EndIteration iid

    inTargIds :: i -> Bool
    inTargIds i = maybe True (S.member (identifier i)) iIds

  in
    case filteredItems of
      [] -> []
      [x] -> [startTest *> runItem x *> endTest]
      x : xs -> (startTest *> runItem x)
                : (runItem <$> Prelude.init xs)
                <> [runItem (Prelude.last xs) *> endTest]

runTest ::  forall i rc as ds tc e effs. (ItemClass i ds, TestConfigClass tc, ToJSON e, Show e, Member (Logger e) effs) =>
                   Maybe (S.Set Int)                                                        -- target Ids
                   -> FilterList rc tc                                                      -- filters
                   -> (ItemParams e as ds i tc rc effs -> Sem effs ())                      -- item runner
                   -> rc                                                                    -- runConfig
                   -> GenericTest e tc rc i as ds effs                                     -- Test Case
                   -> [Sem effs ()]                                                         -- [TestIterations]
runTest iIds fltrs itemRunner rc GenericTest {config = tc, testItems, testInteractor, testPrepState} =
    (acceptFilter $ filterTestCfg fltrs rc tc)
        ? runTestItems iIds (testItems rc) (TestParams testInteractor testPrepState tc rc) itemRunner
        $ []

logLPError ::  forall e effs. (ToJSON e, Show e, Member (Logger e) effs) => FrameworkError e -> Sem effs ()
logLPError = logLP . logRun . LP.Error

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
          preRunRslt <- PE.catch runAction (PE.throw . rolloverFailError)
          runCheck <- PE.catch checkHasRun (PE.throw . PreTestCheckExecutionError stage "exception encountered verifying hook has run")
          unless runCheck (PE.throw rolloverCheckFalseError)
      
    (Right <$> exeHook) `PE.catch` (pure . Left)


data RunParams e rc tc effs = RunParams {
  plan :: forall a mo mi. TestPlanBase e tc rc mo mi a effs,
  filters :: FilterList rc tc,
  itemRunner :: forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => (ItemParams e as ds i tc rc effs -> Sem effs ()),
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
    logBoundry = logLP . BoundaryLog

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
                   -> TestModule                                      -- test address
                   -> Either FilterErrorType (S.Set Int)                  -- a set of item Ids used for test case endpoints                                               -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> Sem effs ()
mkEndpointSem runParams@RunParams { filters } tstAddress iIds =
  let
    endpointFilter :: TestModule -> TestFilter rc tc
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