

{-# LANGUAGE NoPolyKinds #-} 
-- TODO: work out why this is needed - investigate polykinds

module Runner (
  applyTestFilters
  , docExecution
  , doNothing
  , ItemRunner
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

type TestPlanBase tc rc m1 m a itmEffs = (forall i as ds. (ItemClass i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) => GenericTest tc rc i itmEffs as ds -> m1 (m a)) -> [TestGroup m1 m a itmEffs]

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
                either pPrint (\HandleInfo{..} -> 
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

doNothing :: PreRun itmEffs
doNothing = PreRun {
  runAction = pure (),
  checkHasRun = pure True
}

disablePreRun :: TestGroup m m1 a itmEffs -> TestGroup m m1 a itmEffs
disablePreRun tg = tg {
                        rollover = doNothing,
                        goHome = doNothing
                      }

testAddress :: forall tc rc i itmEffs as ds. TestConfigClass tc => GenericTest tc rc i itmEffs as ds -> TestModule
testAddress =  moduleAddress . (configuration :: GenericTest tc rc i itmEffs as ds -> tc)


-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Run Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type ItemRunner as ds i tc rc itmEffs apEffs = ItemRunParams as ds i tc rc itmEffs apEffs -> Sem apEffs ()                                            

data TestRunParams as ds i tc rc itmEffs apEffs = TestRunParams {  
  logger :: LogProtocol -> Sem apEffs (),                         
  interactor :: rc -> i -> Sem itmEffs as,                         
  prepState :: i -> as -> Ensurable ds,                         
  interpreter :: forall a. Sem itmEffs a -> Sem apEffs (Either AppError a), 
  tc :: tc,                                                     
  rc :: rc                                                      
}

data ItemRunParams as ds i tc rc itmEffs apEffs = ItemRunParams {
  testRunParams :: TestRunParams as ds i tc rc itmEffs apEffs,                                                     
  item :: i                                                        
}

normalExecution :: forall m itmEffs apEffs rc tc i as ds. (ItemClass i ds, ToJSON as, ToJSON ds, TestConfigClass tc, ApEffs apEffs) 
                  => ItemRunner as ds i tc rc itmEffs apEffs
normalExecution (ItemRunParams (TestRunParams logger interactor prepState intrprt tc rc) i)  = 
  let
    iid :: ItemId
    iid = ItemId (moduleAddress tc) (identifier i)

    logRunItem :: RunProtocol -> Sem apEffs ()
    logRunItem = logger . logRun

    logChk :: CK.CheckReport -> Sem apEffs ()
    logChk cr = logRunItem $ CheckOutcome iid cr

    handler :: AppError -> Sem apEffs ()
    handler = logger . logRun . LP.Error 

    recordSkippedChecks :: Sem apEffs ()
    recordSkippedChecks = do 
                            logRunItem StartChecks 
                            F.traverse_ logChk $ D.toList $ CK.skipChecks (checkList i)

    normalExecution' :: Sem apEffs ()
    normalExecution' = 
      let
        runChecks :: ds -> Sem apEffs ()
        runChecks ds = F.traverse_ logChk $ D.toList $ CK.calcChecks ds (checkList i)
      in 
        do 
          logRunItem StartInteraction
          -- TODO: check for io exceptions / SomeException - use throw from test
          ethas <- intrprt $ interactor rc i
        
          eitherf ethas
            (\e -> logRunItem (InteractorFailure iid e) *> recordSkippedChecks)
            (\as -> 
                do 
                  logRunItem . InteractorSuccess iid . ApStateJSON . toJSON $ as
                  
                  let 
                    eds :: Either EnsureError ds
                    eds = fullEnsureInterpreter $ prepState i as
                  
                  logRunItem StartPrepState
                  eitherf eds
                    (\e -> (logRunItem . PrepStateFailure iid $ AppEnsureError e) *> recordSkippedChecks)
                    (
                      \ds -> 
                        do
                          logRunItem . PrepStateSuccess iid . DStateJSON . toJSON $ ds
                          logRunItem StartChecks
                          runChecks ds
                    )
            )
  in 
    PE.catch
      normalExecution'
      handler

docExecution :: forall apEffs itmEffs rc tc i as ds. (ItemClass i ds, TestConfigClass tc)
              => ItemRunner as ds i tc rc itmEffs apEffs
docExecution (ItemRunParams (TestRunParams logger interactor prepState intrprt tc rc) i) = 
  let
    iid :: ItemId
    iid = ItemId (moduleAddress tc) $ identifier i

    docLog :: DocProtocol -> Sem apEffs ()
    docLog = logger . logDoc

    logChecks :: Sem apEffs ()
    logChecks =  P.sequence_ $  (\chk -> docLog $ DocCheck iid (CK.header (chk :: CK.Check ds)) (CK.expectation chk) (CK.gateStatus chk)) <$> D.toList (checkList i)
  in 
    do 
      docLog DocInteraction
      intrprt (interactor rc i)
      docLog DocChecks
      logChecks

runTestItems :: forall i as ds tc rc itmEffs apEffs. (TestConfigClass tc, ItemClass i ds) =>
      Maybe (S.Set Int)                                                    -- target Ids
      -> [i]                                                               -- items
      -> TestRunParams as ds i tc rc itmEffs apEffs
      -> ItemRunner as ds i tc rc itmEffs apEffs
      -> [Sem apEffs ()]
runTestItems iIds items runPrms@(TestRunParams logger interactor prepState intrprt tc rc) itemRunner =
  let
    logBoundry :: BoundaryEvent -> Sem apEffs ()
    logBoundry = logger . BoundaryLog

    startTest :: Sem apEffs ()
    startTest = logBoundry . StartTest $ mkDisplayInfo tc

    endTest :: Sem apEffs ()
    endTest = logBoundry . EndTest $ moduleAddress tc

    filteredItems :: [i]
    filteredItems = filter inTargIds items

    runItem :: i -> Sem apEffs ()
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

runTest ::  forall i rc as ds tc itmEffs apEffs. (ItemClass i ds, TestConfigClass tc, Member Logger itmEffs) =>
                   Maybe (S.Set Int)                                                        -- target Ids
                   -> FilterList rc tc                                                      -- filters
                   -> ItemRunner as ds i tc rc itmEffs apEffs                               -- item runner
                   -> rc                                                                    -- runConfig
                   -> (forall a. Sem itmEffs a -> Sem apEffs (Either AppError a))           -- interpreter
                   -> GenericTest tc rc i itmEffs as ds                                     -- Test Case
                   -> [Sem apEffs ()]                                                        -- [TestIterations]
runTest iIds fltrs itemRunner rc intrprt GenericTest{configuration = tc, components} =
  let
    logPrtcl :: LogProtocol -> Sem apEffs ()
    logPrtcl = logger' intrprt

    runItems :: TestComponents rc i itmEffs as ds -> [Sem apEffs ()]
    runItems TestComponents {testItems, testInteractor, testPrepState} = runTestItems iIds (testItems rc) (TestRunParams logPrtcl testInteractor testPrepState intrprt tc rc) itemRunner

    include :: Bool
    include = acceptFilter $ filterTestCfg fltrs rc tc
  in
    include
        ? runItems components
        $ []

logger' :: forall itmEffs apEffs. (Member Logger itmEffs) =>
                 (forall a. Sem itmEffs a -> Sem apEffs (Either AppError a)) -- interpreter
                 -> LogProtocol
                 -> Sem apEffs ()
logger' intrprt = void . intrprt . logItem

testRunOrEndpoint :: forall rc tc itmEffs apEffs. (RunConfigClass rc, TestConfigClass tc, EFFLogger itmEffs) =>
                    Maybe (S.Set Int)                                   -- a set of item Ids used for test case endpoints
                   -> (forall a mo mi. TestPlanBase tc rc mo mi a itmEffs) -- test case processor function is applied to a hard coded list of test groups and returns a list of results
                   -> FilterList rc tc                                  -- filters
                   -> (forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => ItemRunner as ds i tc rc itmEffs apEffs)                   -- item runner
                   -> (forall a. Sem itmEffs a -> Sem apEffs (Either AppError a))   -- interpreter
                   -> rc                                                -- runConfig
                   -> Sem apEffs ()
testRunOrEndpoint iIds runner fltrs itemRunner intrprt rc =
        let
          preRun :: PreRun itmEffs -> PreTestStage -> Sem apEffs (Either AppError ())
          preRun PreRun{..} stage = 
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

                verifyAction :: Either AppError Bool -> Either AppError ()
                verifyAction  = either
                                          (Left . AppPreTestCheckExecutionError stage (msgPrefix <> stageExLabel <> " check"))
                                          (\hmChk -> hmChk ?
                                                        Right () $
                                                        Left
                                                            $ AppPreTestCheckError stage
                                                              $ msgPrefix
                                                              <> stageStr
                                                              <> " action ran without exception but completion check returned False. Looks like "
                                                              <> stageStr
                                                              <> " did not run as expected"
                                          )

              preRunRslt <- intrprt runAction
              runCheck <- intrprt checkHasRun
              pure $ either
                        (Left . AppPreTestError stage stageExLabel)
                        (\_ -> verifyAction runCheck)
                        preRunRslt

          filterInfo :: [[FilterResult]]
          filterInfo = filterGroups runner fltrs rc

          filterFlags :: [Bool]
          filterFlags = filterGroupFlags filterInfo

          prepResults :: [TestGroup [] (Sem apEffs) () itmEffs]
          prepResults = runner $ runTest iIds fltrs itemRunner rc intrprt

          firstDuplicateGroupTitle :: Maybe Text
          firstDuplicateGroupTitle = toS <$> firstDuplicate (toS . C.title <$> prepResults :: [Prelude.String])

          runTuples ::  [(Bool, TestGroup [] (Sem apEffs) () itmEffs)]
          runTuples = P.zip filterFlags prepResults
          
          logPtcl :: LogProtocol -> Sem apEffs ()
          logPtcl = logger' intrprt
          
          logBoundry :: BoundaryEvent -> Sem apEffs ()
          logBoundry = logPtcl . BoundaryLog

          logLPError :: AppError -> Sem apEffs ()
          logLPError = logPtcl . logRun . LP.Error

          exeGroup :: (Bool, TestGroup [] (Sem apEffs) () itmEffs) -> Sem apEffs ()
          exeGroup (include, tg) =
            let
              -- when running an endpoint go home and rolllover are not run
              -- if the application is already home
              isEndpoint :: Bool
              isEndpoint = isJust iIds

              preRunGuard ::  Sem apEffs (Either AppError Bool)
              preRunGuard = (
                              isEndpoint ?
                                  intrprt (not <$> checkHasRun (goHome tg)) $ -- we only want to run if is NOT already home
                                  pure $ Right True
                            )

              guardedPreRun :: (TestGroup [] (Sem apEffs) () itmEffs -> PreRun itmEffs) -> PreTestStage -> Sem apEffs (Either AppError ())
              guardedPreRun sel stg =
                do
                  wantRun <- preRunGuard
                  either
                    (pure . Left)
                    (bool
                      (pure $ Right ()) $
                      preRun (sel tg) stg
                    )
                    wantRun

              grpRollover :: Sem apEffs (Either AppError ())
              grpRollover = guardedPreRun rollover Rollover

              grpGoHome :: Sem apEffs (Either AppError ())
              grpGoHome = guardedPreRun goHome GoHome

              logFailOrRun :: Sem apEffs (Either AppError ()) -> Sem apEffs () -> Sem apEffs ()
              logFailOrRun prerun mRun = do
                                          pr <- prerun
                                          either logLPError (const mRun) pr

              runTestIteration :: Sem apEffs () -> Sem apEffs ()
              runTestIteration = logFailOrRun grpGoHome

              runTest' :: [Sem apEffs ()] -> Sem apEffs ()
              runTest' testIterations = sequence_ (runTestIteration <$> testIterations)

              testList :: [[Sem apEffs ()]]
              testList = filter (not . null) $ tests tg

              runGroupAfterRollover :: Sem apEffs ()
              runGroupAfterRollover = sequence_ $ runTest' <$> testList

              runGrp :: Sem apEffs ()
              runGrp = 
                    let 
                      hdr = GroupTitle $ RB.header tg
                    in
                      do
                        logBoundry $ StartGroup hdr
                        logFailOrRun grpRollover runGroupAfterRollover
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
          

testRun :: forall rc tc m itmEffs apEffs. (RunConfigClass rc, TestConfigClass tc, EFFLogger itmEffs) =>
            (forall a mo mi. TestPlanBase tc rc mo mi a itmEffs) -- test case processor function is applied to a hard coded list of test groups and returns a list of results
            -> FilterList rc tc                                  -- filters
            -> (forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => ItemRunner as ds i tc rc itmEffs apEffs)                   -- item runner
            -> (forall a. Sem itmEffs a -> Sem apEffs (Either AppError a))   -- interpreter
            -> rc                                                -- runConfig
            -> Sem apEffs ()
testRun = testRunOrEndpoint Nothing

testEndpointBase :: forall rc tc itmEffs apEffs. (RunConfigClass rc, TestConfigClass tc, EFFLogger itmEffs) =>
                   FilterList rc tc                               -- filters
                   -> (forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => ItemRunner as ds i tc rc itmEffs apEffs)  
                   -> (forall a. Sem itmEffs a -> Sem apEffs (Either AppError a)) -- interpreter
                   -> TestModule                                      -- test address
                   -> rc                                              -- runConfig
                   -> Either FilterError (S.Set Int)                  -- a set of item Ids used for test case endpoints
                   -> (forall a mo mi. TestPlanBase tc rc mo mi a itmEffs)  -- test case processor function is applied to a hard coded list of test goups and returns a list of results                                                -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> Sem apEffs ()
testEndpointBase fltrs itemRunner intrprt tstAddress rc iIds runner =
  let
    logPtcl :: LogProtocol -> Sem apEffs ()
    logPtcl = logger' intrprt

    endpointFilter :: TestModule -> TestFilter rc tc
    endpointFilter targAddress = TestFilter {
      title = "test address does not match endpoint target: " <> toString targAddress,
      predicate = \_ tc -> moduleAddress tc == targAddress
    }

    allFilters :: [TestFilter rc tc]
    allFilters = endpointFilter tstAddress : fltrs
  in
    eitherf iIds
      (logPtcl . logRun . LP.Error . AppFilterError)
      (\idSet -> testRunOrEndpoint (Just idSet) runner allFilters itemRunner intrprt rc)