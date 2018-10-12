
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

type FullRunner = forall rc tc i as vs effs. (ItemClass i vs, Show i, Show as, Show vs, EFFFileSystem effs) => GenericTest rc tc i (Eff effs as) as vs -> IO ()

data GenericTest testConfig runConfig item effs apState valState = Test {
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

runTest :: forall rslt i vs effs rc testConfig as ag. (ItemClass i vs) =>
                            rc                                              -- runConfig
                            -> (GenericResult testConfig rslt -> IO ())     -- logger
                            -> (i -> as -> vs -> ag)                        -- aggreagator - a constructor for the final result type
                            -> ((as -> ag) -> Eff effs as -> IO rslt)       -- interpreter
                            -> Filter i                                     -- item filter
                            -> GenericTest testConfig rc i (Eff effs as) as vs
                            -> IO ()
runTest runConfig logger aggregator interpreter filtr Test {..} = let
                                                              flipResult :: Either FilterError (IO [rslt]) -> IO (Either FilterError [rslt])
                                                              flipResult = \case
                                                                              Left fe -> pure $ Left fe
                                                                              Right ioR -> Right <$> ioR

                                                              rslts :: IO (Either FilterError [rslt])
                                                              rslts = flipResult $
                                                                      P.sequenceA <$>
                                                                      runSteps aggregator runConfig components interpreter filtr
                                                            in
                                                              do
                                                                rslt <- TestResult address configuration <$> rslts
                                                                logger rslt

runSteps :: (ItemClass i vs) =>
                            (i -> as -> vs -> ag)                           -- aggreagator - a constructor for the final result type
                            -> rc                                           -- runConfig
                            -> TestComponents rc i (Eff effs as) as vs      -- items / interactor / prepState
                            -> ((as -> ag) -> Eff effs as -> IO rslt)       -- interpreter
                            -> Filter i                                     -- item filter
                            -> Either FilterError [IO rslt]
runSteps aggregator runConfig TestComponents {..} interpreter filtr =
    let
      apStateToValState i a = aggregator i a (testPrepState a)
      interactorEffects = testInteractor runConfig
      itemToResult i = interpreter (apStateToValState i) (interactorEffects i)
    in
      (itemToResult <$>) <$> filterredItems filtr testItems

data TestInfo i as vs = TestInfo {
  item :: i,
  apState  :: Maybe as,
  valState :: Maybe vs,
  checkResult :: Maybe CheckResultList
} deriving Show

testInfoFull :: ItemClass i vs => i -> as -> vs -> TestInfo i as vs
testInfoFull item apState valState =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Just valState,
      checkResult = Just $ calcChecks valState $ checkList item
    }

testInfoNoValidation :: i -> a -> v -> TestInfo i a v
testInfoNoValidation item apState _ =
  TestInfo {
      item = item,
      apState = Just apState,
      valState = Nothing,
      checkResult = Nothing
    }

runStepsNoValidation :: (ItemClass i vs) =>  rc                                  -- runConfig
                                        -> TestComponents rc i (Eff effs as) as vs
                                        -> ((as -> TestInfo i as vs) -> Eff effs as -> IO rslt)  -- interpreter
                                        -> Filter i                                              -- item filter
                                        -> Either FilterError [IO rslt]
runStepsNoValidation = runSteps testInfoNoValidation
