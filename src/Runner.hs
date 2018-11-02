
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
import TestAndRunConfig
import           Control.Monad.Freer
import           Control.Monad.Freer.Error

import           Foundation.Extended
import           Runner.Internal
import           Runner.Internal     as InternalFuncs (Filter (..),
                                                       FilterError (..))
import           ItemClass
import qualified Prelude             as P
import           DSL.Interpreter
import Data.Either
import Control.Monad


data TestComponents rc i effs as vs = TestComponents {
  testItems :: [i],
  testInteractor :: rc -> i -> Eff effs as,
  testPrepState :: as -> Ensurable vs
}

data TestHeaderData tc = TestHeaderData {
  address :: String,
  configuration :: tc
} deriving (Eq, Show)

data GenericTest tc rc i effs as vs = GenericTest {
  headerData :: TestHeaderData tc,
  components :: ItemClass i vs => TestComponents rc i effs as vs
}

data GenericResult testConfig rslt = TestResult {
  address :: String,
  configuration :: testConfig,
  results :: Either FilterError [rslt]
} deriving Show

data TestInfo i as vs = TestInfo {
                                  item :: i,
                                  apState  :: as,
                                  valState :: vs,
                                  checkResult :: CheckResultList
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

-- forall effs a. Member (Error EnsureError) effs => Eff (Ensure ': effs) a
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

runAllItems :: (Functor f1, Functor f2) =>
      [i]                                                      -- items
      -> (rc -> i -> Eff effs as)                              -- interactor
      -> (as -> Ensurable vs)                                    -- prepstate
      -> (i -> f2 (TestInfo i as vs) -> TestInfo i as vs)  -- recover from either
      -> (i -> as -> vs -> TestInfo i as vs)                 -- aggragator
      -> rc                                                      -- runconfig
      -> (Eff effs as -> f1 (f2 as))                             -- interpreter
      -> [f1 (TestInfo i as vs)]
runAllItems items interactor prepState frmEth agg rc intrprt = (\itm -> frmEth itm <$> runApState interactor prepState agg rc intrprt itm) <$> items

runLogAll ::  forall i rc as vs m tc effs. (Monad m, ItemClass i vs, Show i, Show as, Show vs, Member Logger effs) =>
                   (i -> as -> vs -> TestInfo i as vs)     -- aggregator i.e. rslt constructor
                   -> rc                                       -- runConfig
                   -> (forall a. Eff effs a -> m (Either AppError a))  -- interpreter
                   -> GenericTest tc rc i effs as vs         -- Test Case
                   -> [m ()]
runLogAll agg rc intrprt tst =
        let
          logger = void . intrprt . log
          result TestComponents{..} = (logger =<<) <$> runAllItems testItems testInteractor testPrepState recoverTestInfo agg rc intrprt
        in
          result $ components tst

genericTestRun :: forall effs m rc tc. (EFFFileSystem effs, Monad m, Show tc) =>
                  (forall mr m1 a. (forall i as vs. (ItemClass i vs, Show i, Show as, Show vs) => GenericTest tc rc i effs as vs -> m1 (mr a)) -> [m1 (mr a)])
                  -> TestFilters rc tc                                          -- genericTest filters
                  -> rc                                                              -- runConfig
                  -> (forall i as vs. (ItemClass i vs) => i -> as -> vs -> TestInfo i as vs)  -- aggregator (result constructor)
                  -> (forall a. Eff effs a -> m (Either AppError a))                        -- interpreter
                  -> m ()
genericTestRun runner fltrs r agg itpr =
                      let
                        filterTests' :: (forall i as vs. TestFilters rc tc -> rc -> GenericTest tc rc i effs as vs -> Identity (TestFilterResult tc)) -> [TestFilterResult tc]
                        filterTests' ff = filterTests runner ff fltrs r

                        fltrLog :: [TestFilterResult tc]
                        fltrLog = filterTests' filterTest

                        log' = itpr . log
                      in
                        log' fltrLog >> foldl' (>>) (pure ()) (P.concat $ runner $ runLogAll agg r itpr)

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Filtering Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type Reason = String
data FilterRejection tc = FilterRejection Reason (TestHeaderData tc) deriving Show
type TestFilterResult tc = Either (FilterRejection tc) (TestHeaderData tc)

type TestFilter rc tc = rc -> TestHeaderData tc -> TestFilterResult tc
type TestFilters rc tc = [TestFilter rc tc]

filterTestHdr :: TestFilters rc tc -> rc -> TestHeaderData tc -> TestFilterResult tc
filterTestHdr fltrs rc headr = fromMaybe (pure headr) $ find isLeft $ (\f -> f rc headr) <$> fltrs

filterTest :: forall i as vs tc rc effs. TestFilters rc tc -> rc -> GenericTest tc rc i effs as vs -> Identity (TestFilterResult tc)
filterTest fltrs rc t = Identity $ filterTestHdr fltrs rc $ headerData t

filterTests :: forall effs rc tc.
      ((forall i as vs. GenericTest tc rc i effs as vs -> Identity (TestFilterResult tc)) -> [Identity (TestFilterResult tc)])
      -> (forall i as vs. TestFilters rc tc -> rc -> GenericTest tc rc i effs as vs -> Identity (TestFilterResult tc))
      -> TestFilters rc tc
      -> rc
      -> [TestFilterResult tc]
filterTests runner fltrFunc fltrs rc =  runIdentity <$> runner (fltrFunc fltrs rc)
