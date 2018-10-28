
module Runner (
    module Runner
  , module InternalFuncs
  , module ItemClass
) where

import Check
import DSL.Logger
import DSL.Ensure
import DSL.FileSystem
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
import Data.Functor


data TestComponents runConfig item effs apState valState = TestComponents {
  testItems :: [item],
  testInteractor :: runConfig -> item -> Eff effs apState,
  testPrepState :: apState -> Ensurable valState
}

data GenericTest testConfig runConfig item effs apState valState = GenericTest {
  address :: String,
  configuration :: testConfig,
  components :: ItemClass item valState => TestComponents runConfig item effs apState valState
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
      [itm]                                                      -- items
      -> (rc -> itm -> Eff effs as)                              -- interactor
      -> (as -> Ensurable vs)                                    -- prepstate
      -> (itm -> f2 (TestInfo itm as vs) -> TestInfo itm as vs)  -- recover from either
      -> (itm -> as -> vs -> TestInfo itm as vs)                 -- aggragator
      -> rc                                                      -- runconfig
      -> (Eff effs as -> f1 (f2 as))                             -- interpreter
      -> [f1 (TestInfo itm as vs)]
runAllItems items interactor prepState frmEth agg rc intrprt = (\itm -> frmEth itm <$> runApState interactor prepState agg rc intrprt itm) <$> items

runLogAllItems ::  forall itm rc as vs m b effs. (Monad m, Show itm, Show as, Show vs) =>
                   (rc -> itm -> Eff effs as)                  -- interactor
                   -> (as -> Ensurable vs)                     -- prepstate
                   -> [itm]                                    -- items
                   -> (itm -> as -> vs -> TestInfo itm as vs)  -- aggregator i.e. rslt constructor
                   -> (forall s. Show s => s -> m b)           -- logger
                   -> rc                                       -- runConfig
                   -> (Eff effs as -> m (Either AppError as))  -- interpreter
                   -> [m b]
runLogAllItems interactor prepstate itms agg logger rc intrprt = (logger =<<) <$> runAllItems itms interactor prepstate recoverTestInfo agg rc intrprt

runLogAll ::  forall itm rc as vs m b tc effs. (Monad m, ItemClass itm vs) =>
                   (itm -> as -> vs -> TestInfo itm as vs)     -- aggregator i.e. rslt constructor
                   -> (TestInfo itm as vs -> m b)              -- logger
                   -> rc                                       -- runConfig
                   -> (Eff effs as -> m (Either AppError as))  -- interpreter
                   -> GenericTest tc rc itm effs as vs         -- Test Case
                   -> [m b]
runLogAll agg logger rc intrprt tst =
        let
          result TestComponents{..} = (logger =<<) <$> runAllItems testItems testInteractor testPrepState recoverTestInfo agg rc intrprt
        in
          result $ components tst
