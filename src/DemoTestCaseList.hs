module DemoTestCaseList where

import           Control.Monad.Freer
import           DemoConfig
import           DemoRoughTest
import           DemoRoughTestSimple
import           DSL.Interpreter
import           DSL.FileSystem
import           DSL.Ensure
import           Control.Monad.Freer.Error
import           Foundation.Extended
import           Runner
import qualified Prelude as P

runAllFull :: (ItemClass i vs, Show i, Show as, Show vs) => Test i (Eff '[FileSystem, Ensure, Error FileSystemError, Error EnsureError, IO] as) as vs -> TestResult (IO (Either AppError String))
runAllFull = testRunnerFull executeFileSystemInIO All

testRun :: [TestResult (IO (Either AppError String))]
testRun = [
  runAllFull DemoRoughTest.test,
  runAllFull DemoRoughTestSimple.test
  ]
