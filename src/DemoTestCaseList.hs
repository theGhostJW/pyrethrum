module DemoTestCaseList where

import           Control.Monad.Freer
import           DemoConfig
import           DemoRoughTest
import           DemoRoughTestSimple
import           DSL.Interpreter
import           DSL.FileSystem
import           DSL.Logger
import           DSL.Ensure
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           Foundation.Extended
import           Runner
import qualified Prelude as P

runAllFull :: (ItemClass i vs, Show i, Show as, Show vs) => Test i (Eff '[FileSystem, Logger, Ensure, Error FileSystemError, Error EnsureError, IO] as) as vs -> IO ()
runAllFull = testRunnerFull executeFileSystemInIO All

runIOList :: [IO ()] -> IO ()
runIOList = foldl' (>>) (pure ())

testRun :: IO ()
testRun = runIOList [
  runAllFull DemoRoughTest.test,
  runAllFull DemoRoughTestSimple.test
  ]

runAllDoc :: (ItemClass i vs, Show i, Show as, Show vs) => Test i  (Eff '[FileSystem, Logger, Ensure, Error EnsureError, Writer [String], IO] as) as vs -> IO ()
runAllDoc = testRunnerFull executeFileSystemDocument All

testRunDoc :: IO ()
testRunDoc = runIOList [
  runAllDoc DemoRoughTest.test,
  runAllDoc DemoRoughTestSimple.test
  ]
