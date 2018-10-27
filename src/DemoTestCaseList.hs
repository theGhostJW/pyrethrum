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

testRun :: forall effs. (EFFFileSystem effs) =>
                  (forall s. Show s => s -> IO ())                                          -- logger
                  -> RunConfig                                                              -- runConfig
                  -> (forall i as vs. ItemClass i vs => i -> as -> vs -> TestInfo i as vs)  -- aggregator (result constructor)
                  -> (forall a. Eff effs a -> IO (Either AppError a))                       -- interpreter
                  -> IO ()
testRun logger runCfg agf interpreter =
                            let
                              runIOList :: [IO ()] -> IO ()
                              runIOList = foldl' (>>) (pure ())
                            in
                              runIOList $ runIOList <$> [
                                                RT.runLogAllItems agf logger runCfg interpreter,
                                                DemoRoughTestSimple.runLogAllItems agf logger runCfg interpreter
                                              ]

runInIO = testRun consoleLogger runConfig testInfoFull executeInIO
runDocument  = testRun consoleLogger runConfig testInfoFull executeDocument
