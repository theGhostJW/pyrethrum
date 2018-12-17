
module Runner (
    module Runner
  , module InternalFuncs
  , module ItemClass
) where

import Check
import DSL.Logger
import DSL.Ensure
import Data.Functor.Identity
import DSL.FileSystem
import qualified Data.Function as F
import           Control.Monad.Freer
import           Control.Monad.Freer.Error

import           Foundation.Extended
import           Runner.Internal.ItemFilters     as InternalFuncs (ItemFilter (..),
                                                       FilterError (..),
                                                       filterredItemIds)
import           ItemClass
import qualified Prelude             as P
import           DSL.Interpreter
import Data.Either
import qualified Data.Set as S
import TestAndRunConfig as C
import Control.Monad

type TestPlan tc rc effs m1 m a = (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest tc rc i effs as vs -> m1 (m a)) -> [TestGroup m1 m a effs]

data PreRun effs = PreRun {
  runAction :: Eff effs (),
  checkHasRun :: Eff effs Bool
}

doNothing :: PreRun effs
doNothing = PreRun {
  runAction = pure (),
  checkHasRun = pure True
}


data TestGroup m1 m a effs =
  TestGroup {
        -- occurs once on client before group is run
        rollover :: PreRun effs,
        -- occurs once before test iteration is run
        goHome :: PreRun effs,
        -- a list of tests
        tests :: [m1 (m a)]
        -- eg [IO Either (AppError TestInfo)]
   }


disablePreRun :: TestGroup m m1 a effs -> TestGroup m m1 a effs
disablePreRun tg = tg {
                        rollover = doNothing,
                        goHome = doNothing
                      }


data TestComponents rc i effs as vs = TestComponents {
  testItems :: [i],
  testInteractor :: rc -> i -> Eff effs as,
  testPrepState :: as -> Ensurable vs
}

data  GenericTest tc rc i effs as vs = GenericTest {
  configuration :: tc,
  components :: ItemClass i vs => TestComponents rc i effs as vs
}

testAddress :: forall tc rc i effs as vs. TestConfigClass tc => GenericTest tc rc i effs as vs -> String
testAddress = moduleAddress . (configuration :: GenericTest tc rc i effs as vs -> tc)

data GenericResult tc rslt = TestResult {
  configuration :: tc,
  results :: Either FilterError [rslt]
} deriving Show

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

testInfoNoValidation :: i -> a -> v -> TestInfo i a v
testInfoNoValidation item apState _ =
  DocInfo {
      item = item,
      apState = apState
    }

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Run Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

runApState :: (Functor f1, Functor f2) =>
     (rc -> itm -> Eff effs as)
     -> (as -> Ensurable vs)  -- prepstate
     -> (itm -> as -> vs -> TestInfo itm as vs)
     -> rc
     -> (Eff effs as -> f1 (f2 as))
     -> itm
     -> f1 (f2 (TestInfo itm as vs))
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

runTestItems :: forall i as vs rc effs f1 f2. (Functor f1, Functor f2, ItemClass i vs) =>
      Maybe (S.Set Int)                                         -- target Ids
      -> [i]                                                    -- items
      -> (rc -> i -> Eff effs as)                               -- interactor
      -> (as -> Ensurable vs)                                   -- prepstate
      -> (i -> f2 (TestInfo i as vs) -> TestInfo i as vs)       -- recover from either
      -> (i -> as -> vs -> TestInfo i as vs)                    -- aggragator
      -> rc                                                     -- runconfig
      -> (Eff effs as -> f1 (f2 as))                            -- interpreter
      -> [f1 (TestInfo i as vs)]
runTestItems iIds items interactor prepState frmEth agg rc intrprt =
  let
    runItem :: i -> f1 (TestInfo i as vs)
    runItem itm = frmEth itm <$> runApState interactor prepState agg rc intrprt itm

    inTargIds :: i -> Bool
    inTargIds i = maybe True (S.member (identifier i)) iIds

  in
    runItem <$> filter inTargIds items

runTest ::  forall i rc as vs m tc effs. (Monad m, ItemClass i vs, Show i, Show as, Show vs, Member Logger effs) =>
                   Maybe (S.Set Int)                                  -- target Ids
                   -> TestFilters rc tc                                  -- filters
                   -> (i -> as -> vs -> TestInfo i as vs)             -- aggregator i.e. rslt constructor
                   -> rc                                              -- runConfig
                   -> (forall a. Eff effs a -> m (Either AppError a)) -- interpreter
                   -> GenericTest tc rc i effs as vs                  -- Test Case
                   -> [m ()]                                          -- [TestIterations]
runTest iIds fltrs agg rc intrprt GenericTest{..} =
        let
          log' :: Show s => s -> m ()
          log' = logger' intrprt

          runItems :: TestComponents rc i effs as vs -> [m ()]
          runItems TestComponents{..} = (log' =<<) <$> runTestItems iIds testItems testInteractor testPrepState recoverTestInfo agg rc intrprt

          include :: Bool
          include = isRight $ filterTestCfg fltrs rc configuration
        in
          include
              ? ((log' "Start Iteration" *>) <$> runItems components)
              $ pure $ pure ()


logger' :: forall m s effs. (Monad m, Show s, Member Logger effs) =>
                 (forall a. Eff effs a -> m (Either AppError a)) -- interpreter
                 -> s
                 -> m ()
logger' intrprt = void . intrprt . log

testRunOrEndpoint :: forall rc tc m effs. (Monad m, Show tc, EFFLogger effs) =>
                    Maybe (S.Set Int)                                   -- a set of item Ids used for test case endpoints
                    -> (
                      forall a mo mi.
                        (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) =>  GenericTest tc rc i effs as vs -> mo (mi a)) -> [TestGroup mo mi a effs]
                    )                                                 -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> TestFilters rc tc                               -- filters
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
                                                           (Left . PreTestCheckExecutionError stage (msgPrefix <> stageExLabel <> " check"))
                                                           (\hmChk -> hmChk ?
                                                                          Right () $
                                                                          Left
                                                                              $ PreTestCheckError stage
                                                                                $ msgPrefix
                                                                                <> stageStr
                                                                                <> " action ran without exception but completion check returned False. Looks like "
                                                                                <> stageStr
                                                                                <> " did not run as expected"
                                                           )

                                preRunRslt <- intrprt runAction
                                runCheck <- intrprt checkHasRun
                                pure $ either
                                         (Left . PreTestError stage stageExLabel)
                                         (\_ -> verifyAction runCheck)
                                         preRunRslt

          log' :: Show s => s -> m ()
          log' = logger' intrprt

          filterInfo :: [[Either (FilterRejection tc) tc]]
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
              preRunGuard = isEndpoint ? intrprt (checkHasRun $ goHome tg) $ pure $ Right True

              guardedPreRun :: (TestGroup [] m () effs -> PreRun effs) -> PreTestStage -> m (Either AppError ())
              guardedPreRun sel stg =
                do
                  wantRun <- preRunGuard
                  either
                    (pure . Left)
                    (bool (pure $ Right ()) $ preRun (sel tg) stg)
                    wantRun

              grpRollover :: m (Either AppError ())
              grpRollover = guardedPreRun rollover Rollover

              grpGoHome :: m (Either AppError ())
              grpGoHome = guardedPreRun goHome GoHome

              logFailOrRun :: m (Either AppError ()) -> m () -> m ()
              logFailOrRun prerun mRun = do
                                         pr <- prerun
                                         either log' (const mRun) pr

              runTestIteration :: m () -> m ()
              runTestIteration = logFailOrRun grpGoHome

              runTest' :: [m ()] -> m ()
              runTest' testIterations = log' "Start Test" *> sequence_ (runTestIteration <$> testIterations)

              testList :: [[m ()]]
              testList = tests tg

              runGroupAfterRollover :: m ()
              runGroupAfterRollover = sequence_ $ runTest' <$> testList

              runGrp :: m ()
              runGrp = log' "Start Group" *> logFailOrRun grpRollover runGroupAfterRollover

           in
              include ? runGrp $ pure ()

        in
          do
            log' "Filter Log"
            log' $ filterLog filterInfo
            sequence_ $ exeGroup <$> runTuples

testRun :: forall rc tc m effs. (Monad m,  Show tc, EFFLogger effs) =>
                    (
                      forall a mo mi.
                        (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) =>  GenericTest tc rc i effs as vs -> mo (mi a)) -> [TestGroup mo mi a effs]
                    )                                                 -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> TestFilters rc tc                               -- filters
                   -> (forall i as vs. (ItemClass i vs, Show i, Show vs, Show as) => i -> as -> vs -> TestInfo i as vs)             -- test aggregator i.e. rslt constructor
                   -> (forall a. Eff effs a -> m (Either AppError a)) -- interpreter
                   -> rc                                              -- runConfig
                   -> m ()
testRun = testRunOrEndpoint Nothing


testEndpointBase :: forall rc tc m effs. (Monad m, Show tc, EFFLogger effs, TestConfigClass tc) =>
                   TestFilters rc tc                               -- filters
                   -> (forall i as vs. (ItemClass i vs, Show i, Show vs, Show as) => i -> as -> vs -> TestInfo i as vs)             -- test aggregator i.e. rslt constructor
                   -> (forall a. Eff effs a -> m (Either AppError a)) -- interpreter
                   -> String                                          -- test address
                   -> rc                                              -- runConfig
                   -> Either FilterError (S.Set Int)                  -- a set of item Ids used for test case endpoints
                   -> (
                     forall a mo mi.
                       (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) =>  GenericTest tc rc i effs as vs -> mo (mi a)) -> [TestGroup mo mi a effs]
                   )                                                 -- test case processor function is applied to a hard coded list of test goups and returns a list of results
                   -> m ()
testEndpointBase fltrs agg intrprt tstAddress rc iIds runner =
  let
    endpointFilter :: String -> TestFilter rc tc
    endpointFilter targAddress = TestFilter {
      title = "test address does not match endpoint target: " <> targAddress,
      predicate = \_ tc -> moduleAddress tc == targAddress
    }

    allFilters :: [TestFilter rc tc]
    allFilters = endpointFilter tstAddress : fltrs
  in
    either
      (logger' intrprt)
      (\idSet -> testRunOrEndpoint (Just idSet) runner allFilters agg intrprt rc)
      iIds



-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Filtering Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


data FilterRejection tc = FilterRejection {
                  reason :: String,
                  cfg :: tc
                  } deriving Show

type TestAddress = String

data TestFilter rc tc = TestFilter {
  title :: String,
  predicate :: rc -> tc -> Bool
}

type TestFilters rc tc = [TestFilter rc tc]

filterTestCfg :: forall rc tc. TestFilters rc tc -> rc -> tc -> Either (FilterRejection tc) tc
filterTestCfg fltrs rc tc =
  let
    applyFilter :: TestFilter rc tc -> Either (FilterRejection tc) tc
    applyFilter fltr = predicate fltr rc tc ?
                                        Right tc $
                                        Left $ FilterRejection (Runner.title fltr) tc
  in
    fromMaybe (pure tc) $ find isLeft $ applyFilter <$> fltrs

filterTest :: forall i as vs tc rc effs.  TestFilters rc tc -> rc -> GenericTest tc rc i effs as vs -> Identity (Either (FilterRejection tc) tc)
filterTest fltrs rc t = Identity $ filterTestCfg fltrs rc $ (configuration :: (GenericTest tc rc i effs as vs -> tc)) t

filterGroups :: forall tc rc effs.
              (
                (forall i as vs. (Show i, Show as, Show vs) =>
                      GenericTest tc rc i effs as vs -> Identity (Either (FilterRejection tc) tc)) -> [TestGroup Identity (Either (FilterRejection tc)) tc effs]
              )
              -> TestFilters rc tc
              -> rc
              -> [[Either (FilterRejection tc) tc]]
filterGroups groupLst fltrs rc =
    let
      testFilter :: GenericTest tc rc i effs as vs -> Identity (Either (FilterRejection tc) tc)
      testFilter = filterTest fltrs rc
    in
      (runIdentity <$>) <$> (tests <$> groupLst testFilter)

filterLog :: forall tc. [[Either (FilterRejection tc) tc]] -> [Either (FilterRejection tc) tc]
filterLog = mconcat

filterGroupFlags :: forall tc. [[Either (FilterRejection tc) tc]] -> [Bool]
filterGroupFlags grpFltrRslts = any isRight <$> grpFltrRslts
