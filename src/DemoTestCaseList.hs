module DemoTestCaseList where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DemoConfig
import           DemoRoughTest as RT
import           DemoRoughTestSimple as ST
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.Logger
import           Foundation.Extended
import qualified Prelude                    as P
import           Runner as R

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

                              r :: (ItemClass itm vs, Show itm, Show as, Show vs) => GenericTest tc RunConfig itm effs as vs -> IO ()
                              r = runIOList . R.runLogAll agf logger runCfg interpreter
                            in
                              runIOList [
                                          r RT.test,
                                          r ST.test
                                        ]

runInIO = testRun consoleLogger runConfig testInfoFull executeInIO
runDocument  = testRun consoleLogger runConfig testInfoFull executeDocument
