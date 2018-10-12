module DemoTestCaseList where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DemoConfig
import           DemoRoughTest
import           DemoRoughTestSimple
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.Logger
import           Foundation.Extended
import qualified Prelude                    as P
import           Runner


-- -- interactor :: Effects effs => (ItemClass Item ValState) => RunConfig -> Item -> Eff effs ApState
-- testRun :: forall i vs as effs. (ItemClass i vs, Show i, Show as, Show vs, EFFFileSystem effs) => (Test i (Eff effs as) as vs -> IO ()) -> IO ()
-- testRun runner = let
--                    testResultList = [
--                      DemoRoughTest.execute runner,
--                      DemoRoughTestSimple.execute runner
--                     ]
--                   in
--                    undefined
-- --runIOList $ (testExecutor runner) <$> testResultList

runIOList :: [IO ()] -> IO ()
runIOList = foldl' (>>) (pure ())

testRun' :: IO ()
testRun' = runIOList [
  runAllFull DemoRoughTest.test,
  runAllFull DemoRoughTestSimple.test
  ]

testRunDoc :: IO ()
testRunDoc = runIOList [
  runAllDoc DemoRoughTest.test,
  runAllDoc DemoRoughTestSimple.test
  ]
