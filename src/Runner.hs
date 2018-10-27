
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
