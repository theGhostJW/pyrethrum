
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

-- type FullRunner = forall rc tc i as vs effs. (ItemClass i vs, Show i, Show as, Show vs, EFFFileSystem effs) => GenericTest rc tc i (Eff effs as) as vs -> IO ()

data GenericTest testConfig runConfig item effs apState valState = GenericTest {
  address :: String,
  configuration :: testConfig,
  components :: TestComponents runConfig item effs apState valState
}

data GenericResult testConfig rslt = TestResult {
  address :: String,
  configuration :: testConfig,
  results :: Either FilterError [rslt]
} deriving Show

data TestComponents runConfig item effs apState valState = TestComponents {
  testItems :: [item],
  testInteractor :: runConfig -> item -> effs,
  testPrepState :: apState -> valState
}

data TestInfo i as vs = TestInfo {
                                  item :: i,
                                  apState  :: Maybe as,
                                  valState :: Maybe vs,
                                  checkResult :: Maybe CheckResultList
                                } |

                         TestFault {
                                    item :: i,
                                    error :: AppError
                                  }
                                  deriving Show

testInfoFull :: forall i as vs. ItemClass i vs => i -> as -> vs -> TestInfo i as vs
testInfoFull item apState valState =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Just valState,
      checkResult = Just $ calcChecks valState $ checkList item
    }

recoverTestInfo :: i -> Either AppError (TestInfo i as vs) -> TestInfo i as vs
recoverTestInfo i = either (TestFault i) id

testInfoNoValidation :: i -> a -> v -> TestInfo i a v
testInfoNoValidation item apState _ =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Nothing,
      checkResult = Nothing
    }

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Rqun Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

runApState :: (Functor f1, Functor f2) =>
     (rc -> itm -> Eff effs as)
     -> (as -> vs)
     -> (itm -> as -> vs -> b)
     -> rc
     -> (Eff effs as -> f1 (f2 as))
     -> itm
     -> f1 (f2 b)
runApState interactor prepState agg rc intrprt itm = let
                                   runVals as = agg itm as $ prepState as
                                in
                                   (runVals <$>) <$> intrprt (interactor rc itm)

runAllItems :: (Functor f1, Functor f2) =>
      [itm]                                   -- items
      -> (rc -> itm -> Eff effs as)           -- interactor
      -> (as -> vs)                           -- prepsatate
      -> (itm -> f2 b -> b)                   -- recover from either
      -> (itm -> as -> vs -> b)               -- aggragator
      -> rc                                   -- runconfig
      -> (Eff effs as -> f1 (f2 as))          -- interpreter
      -> [f1 b]
runAllItems items interactor prepState frmEth agg rc intrprt = (\itm -> frmEth itm <$> runApState interactor prepState agg rc intrprt itm) <$> items

runLogAllItems ::  forall itm rc as vs m b effs. (Monad m) =>
                   (rc -> itm -> Eff effs as)                  -- interactor
                   -> (as -> vs)                               -- prepstate
                   -> [itm]                                    -- items
                   -> (itm -> as -> vs -> TestInfo itm as vs)  -- aggregator i.e. rslt constructor
                   -> (TestInfo itm as vs -> m b)              -- logger
                   -> rc                                       -- runConfig
                   -> (Eff effs as -> m (Either AppError as))  -- interpreter
                   -> [m b]
runLogAllItems interactor prepstate itms agg logger rc intrprt = (logger =<<) <$> runAllItems itms interactor prepstate recoverTestInfo agg rc intrprt
