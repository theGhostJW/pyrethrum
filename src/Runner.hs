{-# LANGUAGE NoPolyKinds #-} 
-- TODO: work out why this is needed - investigate polykinds

module Runner (
  applyTestFilters
  , docExecution
  , doNothing
  , logFileHandles
  , normalExecution
  , showAndLogItems
  , testEndpointBase
  , TestPlanBase
  , TestRunParams
  , ItemRunParams
  , testRun
  , module RB
  , module ItemFilter
  , module C
) where

import qualified Check as CK
import Common
import DSL.Interpreter
import DSL.Logger
import DSL.LogProtocol as LP
import DSL.Ensure
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
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Prelude

type TestPlanBase tc rc m1 m a effs = (forall i as ds. (ItemClass i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) => 
                                                          GenericTest tc rc i as ds effs -> m1 (m a)) -> [TestGroup m1 m a effs]

--- Reapplying test Filters to Items ---

applyTestFilters :: forall i tc rc. TestConfigClass tc => [TestFilter rc tc] -> rc -> (i -> tc) -> [i] -> [i]
applyTestFilters fltrs rc cvtr itms = 
    fst <$> filter (isNothing . snd) (applyTestFiltersToItemsShowReason fltrs rc cvtr itms) 

-- debugging
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

        hndle :: IO (Either AppError HandleInfo)
        hndle = either
                  Left
                  (
                    maybe
                      (Left $ AppUserError "showAndLogList - no Handle returned")
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


data Step a = Step {
                  filePrefix :: Maybe Text,
                  result :: Either AppError [(a, HandleInfo)]
                }

logFileHandles :: forall a. M.Map (Text, FileExt) a -> IO (Either AppError [(a, HandleInfo)])
logFileHandles mpSuffixExt = 
  let
    seed :: IO (Step a)
    seed = pure $ Step {
      filePrefix = Nothing,
      result = Right []
    }

    step :: IO (Step a) -> (Text, FileExt) -> a -> IO (Step a)
    step accum (suff, ext) a = 
      do 
        iStep <- accum
        let 
          iResult = result iStep
          iPrefix = filePrefix iStep 
          
        eitherf iResult 
          (const $ pure iStep)
          (
            \hInfoLst -> 
              do 
                eHandInfo <- logFileHandle iPrefix suff ext
                pure $ eitherf eHandInfo
                        (\ioErr -> (iStep :: Step a) {result = Left $ AppIOError' "Error creating log file" ioErr} )
                        (
                          \hi@HandleInfo{..} -> Step {
                                                  filePrefix = Just prefix,
                                                  result = Right $ hInfoLst <> [(a, hi)]
                                                }
                        )
          )

    finalRslt :: IO (Step a)
    finalRslt = M.foldlWithKey' step seed mpSuffixExt
  in 
     result <$> finalRslt 

doNothing :: PreRun effs
doNothing = PreRun {
  runAction = pure (),
  checkHasRun = pure True
}

disablePreRun :: TestGroup m m1 a effs -> TestGroup m m1 a effs
disablePreRun tg = tg {
                        rollover = doNothing,
                        goHome = doNothing
                      }

testAddress :: forall tc rc i effs as ds. TestConfigClass tc => GenericTest tc rc i effs as ds -> TestModule
testAddress =  moduleAddress . (configuration :: GenericTest tc rc i effs as ds -> tc)


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Run Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                   

data TestRunParams as ds i tc rc effs = TestRunParams {                       
  interactor :: rc -> i -> Sem effs as,                         
  prepState :: forall pEffs. Ensurable pEffs => i -> as -> Sem pEffs ds,                      
  tc :: tc,                                                     
  rc :: rc                                                      
}

data ItemRunParams as ds i tc rc effs = ItemRunParams {
  testRunParams :: TestRunParams as ds i tc rc effs,                                                     
  item :: i                                                        
}

logLP :: Member Logger effs => LogProtocol -> Sem effs ()
logLP = logItem 

logRP :: Member Logger effs => RunProtocol -> Sem effs ()
logRP = logLP . logRun 

normalExecution :: forall effs rc tc i as ds. (ItemClass i ds, ToJSON as, ToJSON ds, TestConfigClass tc, ApEffs effs) 
                  => ItemRunParams as ds i tc rc effs -> Sem effs ()  
normalExecution (ItemRunParams (TestRunParams interactor prepState tc rc) i)  = 
  let
    iid :: ItemId
    iid = ItemId (moduleAddress tc) (identifier i)

    logChk :: CK.CheckReport -> Sem effs ()
    logChk cr = logRP $ CheckOutcome iid cr

    recordSkippedChecks :: Sem effs ()
    recordSkippedChecks = do 
                            logRP StartChecks 
                            F.traverse_ logChk $ D.toList $ CK.skipChecks (checkList i)

    prepStateErrorHandler :: EnsureError -> Sem effs ds
    prepStateErrorHandler e = 
      let 
        err = AppEnsureError e
      in
        do 
          logRP $ PrepStateFailure iid err
          recordSkippedChecks
          PE.throw err
       
    normalExecution' :: Sem effs ()
    normalExecution' = 
      let
        runChecks :: ds -> Sem effs ()
        runChecks ds = F.traverse_ logChk $ D.toList $ CK.calcChecks ds (checkList i)
      in 
        do 
          logRP StartInteraction
          -- TODO: check for io exceptions / SomeException - use throw from test
          as <- interactor rc i
          logRP . InteractorSuccess iid . ApStateJSON . toJSON $ as
          ds <- PE.catch (prepState i as) prepStateErrorHandler
           
          logRP . PrepStateSuccess iid . DStateJSON . toJSON $ ds
          logRP StartChecks
          runChecks ds
  in 
    PE.catch
      normalExecution'
      (\case 
          AppEnsureError e -> pure ()
          e -> logRP $ LP.Error e
      )

docExecution :: forall effs rc tc i as ds. (ItemClass i ds, TestConfigClass tc, Member Logger effs)
              => ItemRunParams as ds i tc rc effs -> Sem effs () 
docExecution (ItemRunParams (TestRunParams interactor prepState tc rc) i) = 
  let
    iid :: ItemId
    iid = ItemId (moduleAddress tc) $ identifier i

    docLog :: DocProtocol -> Sem effs ()
    docLog = logLP . logDoc

    logChecks :: Sem effs ()
    logChecks =  P.sequence_ $  (\chk -> docLog $ DocCheck iid (CK.header (chk :: CK.Check ds)) (CK.expectation chk) (CK.gateStatus chk)) <$> D.toList (checkList i)
  in 
    do 
      docLog DocInteraction
      interactor rc i
      docLog DocChecks
      logChecks

runTestItems :: forall i as ds tc rc effs. (TestConfigClass tc, ItemClass i ds, Member Logger effs) =>
      Maybe (S.Set Int)                                                    -- target Ids
      -> [i]                                                               -- items
      -> TestRunParams as ds i tc rc effs
      -> (ItemRunParams as ds i tc rc effs -> Sem effs ())
      -> [Sem effs ()]
runTestItems iIds items runPrms@(TestRunParams interactor prepState tc rc) itemRunner =
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
                      itemRunner $ ItemRunParams runPrms i
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

runTest ::  forall i rc as ds tc effs. (ItemClass i ds, TestConfigClass tc, Member Logger effs) =>
                   Maybe (S.Set Int)                                                        -- target Ids
                   -> FilterList rc tc                                                      -- filters
                   -> (ItemRunParams as ds i tc rc effs -> Sem effs ())                                 -- item runner
                   -> rc                                                                    -- runConfig
                   -> GenericTest tc rc i  as ds effs                                      -- Test Case
                   -> [Sem effs ()]                                                        -- [TestIterations]
runTest iIds fltrs itemRunner rc GenericTest{configuration = tc, components} =
  let
    runItems :: TestComponents rc i as ds effs -> [Sem effs ()]
    runItems TestComponents {testItems, testInteractor, testPrepState} = 
      runTestItems iIds (testItems rc) (TestRunParams testInteractor testPrepState tc rc) itemRunner

    include :: Bool
    include = acceptFilter $ filterTestCfg fltrs rc tc
  in
    include
        ? runItems components
        $ []

preRun :: forall effs. Member (Error AppError) effs => PreRun effs -> PreTestStage -> Sem effs ()
preRun PreRun{runAction, checkHasRun} stage = 
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

      rolloverFailError :: AppError -> AppError
      rolloverFailError = AppPreTestCheckExecutionError stage $ msgPrefix <> stageExLabel <> " check"

      rolloverCheckFalseError :: AppError
      rolloverCheckFalseError = AppPreTestCheckError stage
                                  $ msgPrefix
                                  <> stageStr
                                  <> " action ran without exception but completion check returned False. Looks like "
                                  <> stageStr
                                  <> " did not run as expected"
      
    preRunRslt <- PE.catch runAction (PE.throw . rolloverFailError)
    runCheck <- checkHasRun
    unless runCheck (PE.throw rolloverCheckFalseError)

testRunOrEndpoint :: forall rc tc effs. (RunConfigClass rc, TestConfigClass tc, ApEffs effs) =>
                    Maybe (S.Set Int)                                   -- a set of item Ids used for test case endpoints
                   -> (forall a mo mi. TestPlanBase tc rc mo mi a effs) -- test case processor function is applied to a hard coded list of test groups and returns a list of results
                   -> FilterList rc tc                                  -- filters
                   -> rc                                                -- runConfig
                   -> (forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => (ItemRunParams as ds i tc rc effs -> Sem effs ()))                     -- item runner
                   -> Sem effs ()
testRunOrEndpoint iIds runner fltrs rc itemRunner =
  let
    filterInfo :: [[FilterResult]]
    filterInfo = filterGroups runner fltrs rc

    filterFlags :: [Bool]
    filterFlags = filterGroupFlags filterInfo

    prepResults :: [TestGroup [] (Sem effs) () effs]
    prepResults = runner $ runTest iIds fltrs itemRunner rc 

    firstDuplicateGroupTitle :: Maybe Text
    firstDuplicateGroupTitle = toS <$> firstDuplicate (toS . C.title <$> prepResults :: [Prelude.String])

    runTuples ::  [(Bool, TestGroup [] (Sem effs) () effs)]
    runTuples = P.zip filterFlags prepResults

    logBoundry :: BoundaryEvent -> Sem effs ()
    logBoundry = logLP . BoundaryLog

    logLPError :: AppError -> Sem effs ()
    logLPError = logLP . logRun . LP.Error

    exeGroup :: (Bool, TestGroup [] (Sem effs) () effs) -> Sem effs ()
    exeGroup (include, tg) =
      let
        isEndpoint :: Bool
        isEndpoint = isJust iIds

        -- when running an endpoint go home and rolllover are not run
        -- if the application is already home
        preRunGuard :: Sem effs Bool
        preRunGuard = (
                        isEndpoint ?
                            (not <$> checkHasRun (goHome tg)) $ -- we only want to run if is NOT already home
                            pure True
                      )

        guardedPreRun :: (TestGroup [] (Sem effs) () effs -> PreRun effs) -> PreTestStage -> Sem effs ()
        guardedPreRun sel stg =
            preRunGuard >>= bool (pure ()) (preRun (sel tg) stg)

        grpRollover :: Sem effs ()
        grpRollover = guardedPreRun rollover Rollover

        grpGoHome :: Sem effs ()
        grpGoHome = guardedPreRun goHome GoHome

        preRunAndRun :: Sem effs () -> Sem effs () -> Sem effs ()
        preRunAndRun prerun mRun = do
                                    prerun
                                    mRun

        runTestIteration :: Sem effs () -> Sem effs ()
        runTestIteration = preRunAndRun grpGoHome

        runTest' :: [Sem effs ()] -> Sem effs ()
        runTest' testIterations = sequence_ (runTestIteration <$> testIterations)

        testList :: [[Sem effs ()]]
        testList = filter (not . null) $ tests tg

        runGroupAfterRollover :: Sem effs ()
        runGroupAfterRollover = sequence_ $ runTest' <$> testList

        runGrp :: Sem effs ()
        runGrp = 
              let 
                hdr = GroupTitle $ RB.header tg
              in
                do
                  logBoundry $ StartGroup hdr
                  preRunAndRun grpRollover runGroupAfterRollover
                  logBoundry $ EndGroup hdr
      in
        include ? runGrp $ pure ()
  in
    maybef firstDuplicateGroupTitle
    (
      do
        logBoundry . StartRun (RunTitle $ C.title rc) $ toJSON rc
        logBoundry . FilterLog $ filterLog filterInfo
        sequence_ $ exeGroup <$> runTuples
        logBoundry EndRun
    )
    (\dupeTxt -> logLPError . AppGenericError $ "Test Run Configuration Error. Duplicate Group Names: " <> dupeTxt)
          

testRun :: forall rc tc effs. (RunConfigClass rc, TestConfigClass tc, ApEffs effs) =>
            (forall a mo mi. TestPlanBase tc rc mo mi a effs)                                                              -- test case processor function is applied to a hard coded list of test groups and returns a list of results
            -> FilterList rc tc                                                                                               -- filters
            -> rc
            -> (forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => (ItemRunParams as ds i tc rc effs -> Sem effs ()))  -- item runner                                                -- runConfig
            -> Sem effs ()
testRun = testRunOrEndpoint Nothing

testEndpointBase :: forall rc tc effs. (RunConfigClass rc, TestConfigClass tc, ApEffs effs) =>
                   FilterList rc tc                               -- filters
                   -> (forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => (ItemRunParams as ds i tc rc effs -> Sem effs ()))  
                   -> TestModule                                      -- test address
                   -> rc                                              -- runConfig
                   -> Either FilterError (S.Set Int)                  -- a set of item Ids used for test case endpoints
                   -> (forall a mo mi. TestPlanBase tc rc mo mi a effs)  -- test case processor function is applied to a hard coded 
                                                                        -- list of test goups and returns a list of results                                                -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> Sem effs ()
testEndpointBase fltrs itemRunner tstAddress rc iIds runner =
  let
    endpointFilter :: TestModule -> TestFilter rc tc
    endpointFilter targAddress = TestFilter {
      title = "test address does not match endpoint target: " <> toString targAddress,
      predicate = \_ tc -> moduleAddress tc == targAddress
    }

    allFilters :: [TestFilter rc tc]
    allFilters = endpointFilter tstAddress : fltrs
  in
    eitherf iIds
      (logItem . logRun . LP.Error . AppFilterError)
      (\idSet -> testRunOrEndpoint (Just idSet) runner allFilters rc itemRunner)