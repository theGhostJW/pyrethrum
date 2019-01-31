
module Runner (
    module Runner
  , module RB
  , module ItemFilter
  , module ItemClass
) where

import Check
import DSL.Common
import DSL.LogProtocol as LP
import DSL.Logger
import DSL.Ensure
import           Control.Monad.Freer
import           Foundation.Extended
import           ItemFilter  (ItemFilter (..), filterredItemIds)
import           ItemClass
import qualified Prelude             as P
import           DSL.Interpreter
import qualified Data.Set as S
import TestAndRunConfig as C
import Control.Monad
import Text.Show.Pretty
import qualified System.IO as SIO
import AuxFiles
import DSL.Logger
import OrphanedInstances
import Data.Aeson
import TestFilter
import RunnerBase as RB


type TestPlanBase tc rc m1 m a effs = (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest tc rc i effs as vs -> m1 (m a)) -> [TestGroup m1 m a effs]

showAndLogItems :: Show a => [a] -> IO ()
showAndLogItems = showAndLogList "items"

showAndLogList :: Show a => String -> [a] -> IO ()
showAndLogList logFileSuffix items = 
      let 
        log2Both :: SIO.Handle -> String -> IO ()
        log2Both fileHndl lgStr = putLines SIO.stdout lgStr *> putLines fileHndl lgStr

        listItems :: SIO.Handle -> IO ()
        listItems h = sequence_ $ ((log2Both h) . showPretty) <$> items
      in
        ioActionLogToConsoleAndFile logFileSuffix listItems

ioActionLogToConsoleAndFile :: String -> (SIO.Handle -> IO ()) -> IO ()
ioActionLogToConsoleAndFile logFileSuffix ioAction = 
                    do 
                      ehtPthHdl <- logFileHandle logFileSuffix
                      eitherf ehtPthHdl
                        (putStrLn . show . Left . (AppIOError' "Log file creation failed"))
                        (\(lgFile, h) -> do 
                                            bracket (pure h) SIO.hClose $ ioAction
                                            putStrLn ""
                                            putStrLn $ "Log File: " <> (toStr $ toFilePath lgFile) 
                                            putStrLn ""
                        )

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

testAddress :: forall tc rc i effs as vs. TestConfigClass tc => GenericTest tc rc i effs as vs -> TestModule
testAddress =  moduleAddress . (configuration :: GenericTest tc rc i effs as vs -> tc)

data TestInfo i as vs = TestInfo {
                                  item :: i,
                                  apState  :: as,
                                  valState :: vs,
                                  checkResult :: CheckResultList
                                } |

                         GoHomeError {
                                    item :: i,
                                    error :: AppError
                                  } |

                         InteractorFault {
                                    item :: i,
                                    error :: AppError
                                  } |

                         PrepStateFault {
                                    item :: i,
                                    apState  :: as,
                                    error :: AppError
                                  } |

                         DocInfo {
                                    item :: i,
                                    apState  :: as
                                  }

                                  deriving Show

testInfoFull :: forall i as vs. ItemClass i vs => i -> as -> vs -> TestInfo i as vs
testInfoFull item apState valState =
  TestInfo {
      item = item,
      apState = apState,
      valState = valState,
      checkResult = calcChecks valState $ checkList item
    }

recoverTestInfo :: i -> Either AppError (TestInfo i as vs) -> TestInfo i as vs
recoverTestInfo i = either (InteractorFault i) id

testInfoNoValidation :: i -> as -> vs -> TestInfo i as vs
testInfoNoValidation item apState _ =
  DocInfo {
      item = item,
      apState = apState
    }

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Run Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

runApState :: (Functor f1, Functor f2) =>
     (rc -> i -> Eff effs as)
     -> (as -> Ensurable vs)  -- prepstate
     -> (i -> as -> vs -> TestInfo i as vs)
     -> rc
     -> (Eff effs as -> f1 (f2 as))
     -> i
     -> f1 (f2 (TestInfo i as vs))
runApState interactor prepState agg rc intrprt itm = let
                                                        runVals as =
                                                          let
                                                            ethVs = fullEnsureInterpreter $ prepState as
                                                          in
                                                            either
                                                                (PrepStateFault itm as . AppEnsureError)
                                                                (agg itm as)
                                                                ethVs
                                                     in
                                                        (runVals <$>) <$> intrprt (interactor rc itm)

runTestItems :: forall i as vs tc rc effs m. (Show i, Show as, Show vs, Show tc, Eq tc, Monad m, TestConfigClass tc, ItemClass i vs, Member Logger effs, ToJSON tc, FromJSON tc) =>
      tc
      -> Maybe (S.Set Int)                                                    -- target Ids
      -> [i]                                                                  -- items
      -> (rc -> i -> Eff effs as)                                             -- interactor
      -> (as -> Ensurable vs)                                                 -- prepstate
      -> (i -> Either AppError (TestInfo i as vs) -> TestInfo i as vs)        -- recover from either
      -> (i -> as -> vs -> TestInfo i as vs)                                  -- aggragator
      -> rc                                                                   -- runconfig
      -> (forall a. Eff effs a -> m (Either AppError a))                      -- interpreter
      -> [m ()]
runTestItems tc iIds items interactor prepState frmEth agg rc intrprt =
  let
    logPtcl :: LogProtocol b -> m ()
    logPtcl = logger' intrprt

    runItem :: i -> m ()
    runItem i =  let
                    iid = identifier i
                    modAddress = C.moduleAddress tc
                  in
                    do
                      logPtcl $ StartIteration modAddress iid
                      info <- frmEth i <$> runApState interactor prepState agg rc intrprt i
                      logPtcl $ EndIteration modAddress iid $ showPretty info

    inTargIds :: i -> Bool
    inTargIds i = maybe True (S.member (identifier i)) iIds

    filteredItems :: [i]
    filteredItems = filter inTargIds items

  in
    case filteredItems of
      [] -> []
      x : xs -> (logPtcl (StartTest $ mkDisplayInfo tc) *> runItem x) : (runItem <$> xs)

runTest ::  forall i rc as vs m tc effs. (Monad m, ItemClass i vs, Show i, Show as, Show vs, Show tc, Eq tc, TestConfigClass tc, ToJSON tc, FromJSON tc, Member Logger effs) =>
                   Maybe (S.Set Int)                                  -- target Ids
                   -> FilterList rc tc                               -- filters
                   -> (i -> as -> vs -> TestInfo i as vs)             -- aggregator i.e. rslt constructor
                   -> rc                                              -- runConfig
                   -> (forall a. Eff effs a -> m (Either AppError a)) -- interpreter
                   -> GenericTest tc rc i effs as vs                  -- Test Case
                   -> [m ()]                                          -- [TestIterations]
runTest iIds fltrs agg rc intrprt GenericTest{..} =
        let
          logPtcl :: LogProtocol b -> m ()
          logPtcl = logger' intrprt

          runItems :: TestComponents rc i effs as vs -> [m ()]
          runItems TestComponents{..} = runTestItems configuration iIds testItems testInteractor testPrepState recoverTestInfo agg rc intrprt

          include :: Bool
          include = acceptFilter $ filterTestCfg fltrs rc configuration
        in
          include
              ? runItems components
              $ []



logger' :: forall m b effs. (Member Logger effs, Functor m) =>
                 (forall a. Eff effs a -> m (Either AppError a)) -- interpreter
                 -> LogProtocol b
                 -> m ()
logger' intrprt = void . intrprt . logItem

testRunOrEndpoint :: forall rc tc m effs. (Monad m, Show rc, Eq rc, Eq tc, Titled rc, Show tc, ToJSON tc, FromJSON tc, ToJSON rc, FromJSON rc, TestConfigClass tc, EFFLogger effs) =>
                    Maybe (S.Set Int)                                    -- a set of item Ids used for test case endpoints
                   -> (forall a mo mi. TestPlanBase tc rc mo mi a effs)  -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> FilterList rc tc                                  -- filters
                   -> (forall i as vs. (ItemClass i vs, Show i, Show vs, Show as) => i -> as -> vs -> TestInfo i as vs)             -- test aggregator i.e. rslt constructor
                   -> (forall a. Eff effs a -> m (Either AppError a)) -- interpreter
                   -> rc                                              -- runConfig
                   -> m ()
testRunOrEndpoint iIds runner fltrs agg intrprt rc =
        let
          preRun :: PreRun effs -> PreTestStage ->  m (Either AppError ())
          preRun PreRun{..} stage = do
                                let
                                  stageStr :: String
                                  stageStr = show stage

                                  stageExLabel :: String
                                  stageExLabel = "Execution of " <> stageStr

                                  msgPrefix :: String
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

          logPtcl :: LogProtocol b -> m ()
          logPtcl = logger' intrprt

          filterInfo :: [[FilterResult tc]]
          filterInfo = filterGroups runner fltrs rc

          filterFlags :: [Bool]
          filterFlags = filterGroupFlags filterInfo

          prepResults :: [TestGroup [] m () effs]
          prepResults = runner $ runTest iIds fltrs agg rc intrprt

          runTuples ::  [(Bool, TestGroup [] m () effs)]
          runTuples = P.zip filterFlags prepResults

          exeGroup :: (Bool, TestGroup [] m () effs) -> m ()
          exeGroup (include, tg) =
            let
              -- when running an endpoint go home and rolllover are not run
              -- if the application is already home
              isEndpoint :: Bool
              isEndpoint = isJust iIds

              preRunGuard ::  m (Either AppError Bool)
              preRunGuard = (
                              isEndpoint ?
                                  intrprt (not <$> checkHasRun (goHome tg)) $ -- we only want to run if is NOT already home
                                  pure $ Right True
                            )

              guardedPreRun :: (TestGroup [] m () effs -> PreRun effs) -> PreTestStage -> m (Either AppError ())
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

              grpRollover :: m (Either AppError ())
              grpRollover = guardedPreRun rollover Rollover

              grpGoHome :: m (Either AppError ())
              grpGoHome = guardedPreRun goHome GoHome

              logFailOrRun :: m (Either AppError ()) -> m () -> m ()
              logFailOrRun prerun mRun = do
                                          pr <- prerun
                                          either (logPtcl . LP.Error) (const mRun) pr

              runTestIteration :: m () -> m ()
              runTestIteration = logFailOrRun grpGoHome

              runTest' :: [m ()] -> m ()
              runTest' testIterations = sequence_ (runTestIteration <$> testIterations)

              testList :: [[m ()]]
              testList = filter (not . null) $ tests tg

              runGroupAfterRollover :: m ()
              runGroupAfterRollover = sequence_ $ runTest' <$> testList

              runGrp :: m ()
              runGrp = logPtcl (StartGroup $ RB.header tg) *> logFailOrRun grpRollover runGroupAfterRollover

           in
              include ? runGrp $ pure ()

        in
          do
            logPtcl $ StartRun (C.title rc) rc
            logPtcl $ FilterLog $ filterLog filterInfo
            sequence_ $ exeGroup <$> runTuples
            logPtcl $ EndRun rc

testRun :: forall rc tc m effs. (Monad m, Show rc, Show tc, Eq rc, Eq tc, Titled rc, TestConfigClass tc, ToJSON tc, FromJSON tc, ToJSON rc, FromJSON rc, EFFLogger effs) =>
                   (forall a mo mi. TestPlanBase tc rc mo mi a effs)  -- test case processor function is applied to a hard coded list of test goups and returns a list of results                                                -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> FilterList rc tc                               -- filters
                   -> (forall i as vs. (ItemClass i vs, Show i, Show vs, Show as) => i -> as -> vs -> TestInfo i as vs)             -- test aggregator i.e. rslt constructor
                   -> (forall a. Eff effs a -> m (Either AppError a)) -- interpreter
                   -> rc                                              -- runConfig
                   -> m ()
testRun = testRunOrEndpoint Nothing


testEndpointBase :: forall rc tc m effs. (Monad m, Show rc, Eq rc, Titled rc, Eq tc, Show tc, ToJSON tc, FromJSON tc, ToJSON rc, FromJSON rc, TestConfigClass tc, EFFLogger effs) =>
                   FilterList rc tc                               -- filters
                   -> (forall i as vs. (ItemClass i vs, Show i, Show vs, Show as) => i -> as -> vs -> TestInfo i as vs)             -- test aggregator i.e. rslt constructor
                   -> (forall a. Eff effs a -> m (Either AppError a)) -- interpreter
                   -> TestModule                                          -- test address
                   -> rc                                              -- runConfig
                   -> Either FilterError (S.Set Int)                  -- a set of item Ids used for test case endpoints
                   -> (forall a mo mi. TestPlanBase tc rc mo mi a effs)  -- test case processor function is applied to a hard coded list of test goups and returns a list of results                                                -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> m ()
testEndpointBase fltrs agg intrprt tstAddress rc iIds runner =
  let
    logPtcl :: LogProtocol b -> m ()
    logPtcl = logger' intrprt

    endpointFilter :: TestModule -> TestFilter rc tc
    endpointFilter targAddress = TestFilter {
      title = "test address does not match endpoint target: " <> toString targAddress,
      predicate = \_ tc -> moduleAddress tc == targAddress
    }

    allFilters :: [TestFilter rc tc]
    allFilters = endpointFilter tstAddress : fltrs
  in
    either
      (logPtcl . LP.Error . AppFilterError)
      (\idSet -> testRunOrEndpoint (Just idSet) runner allFilters agg intrprt rc)
      iIds
