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

runInIO = testRun consoleLogger runConfig testInfoFull executeInIO
runDocument  = testRun consoleLogger runConfig testInfoFull executeDocument

testRun :: forall effs m. (EFFFileSystem effs, Monad m) =>
                  (forall s. Show s => s -> m ())                                          -- logger
                  -> RunConfig                                                             -- runConfig
                  -> (forall i as vs. ItemClass i vs => i -> as -> vs -> TestInfo i as vs)  -- aggregator (result constructor)
                  -> (forall a. Eff effs a -> m (Either AppError a))                       -- interpreter
                  -> m ()
testRun = runRunner R.runLogAll

runRunner :: forall effs m. (EFFFileSystem effs, Monad m) =>
                              Runner
                              -> (forall s. Show s => s -> m ())                                        -- logger
                              -> RunConfig                                                              -- runConfig
                              -> (forall i as vs. ItemClass i vs => i -> as -> vs -> TestInfo i as vs)  -- aggregator (result constructor)
                              -> (forall a. Eff effs a -> m (Either AppError a))                       -- interpreter
                              -> m ()
runRunner runner logger runCfg agf interpreter =
  let
    mergeM_ :: [m ()] -> m ()
    mergeM_ = foldl' (>>) (pure ())

    r :: (ItemClass itm vs, Show itm, Show as, Show vs) => GenericTest tc RunConfig itm effs as vs -> m ()
    r = mergeM_ . runner agf logger runCfg interpreter
  in
    mergeM_ [
      r RT.test,
      r ST.test
    ]
