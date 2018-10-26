module DemoTestCaseList where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DemoConfig
import           DemoRoughTest as RT
import           DemoRoughTestSimple
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.Logger
import           Foundation.Extended
import qualified Prelude                    as P
import           Runner

testRun :: forall effs. (EFFFileSystem effs)=>
                  (forall s. Show s => s -> IO ())
                  -> RunConfig
                  -> (forall i as vs. ItemClass i vs => i -> as -> vs -> TestInfo i as vs)
                  -> (forall a. Eff effs a -> IO (Either AppError a))
                  -> IO ()
testRun logger runCfg agf interpreter =
                            let
                              runIOList :: [IO ()] -> IO ()
                              runIOList = foldl' (>>) (pure ())
                            in
                              runIOList $ runIOList <$> [
                                                RT.runLogAllItems logger agf runCfg interpreter,
                                                DemoRoughTestSimple.runLogAllItems logger agf runCfg interpreter
                                              ]

runInIO = testRun consoleLogger runConfig testInfoFull executeInIO
runDocument  = testRun consoleLogger runConfig testInfoFull executeDocument
