
module DSL.Interpreter where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DSL.FileSystem
import           DSL.Ensure
import           Foundation.Extended
import TestItem
import AppError

type InteractorFileSystem runConfig item r = forall effs. (Members '[Ensure, FileSystem] effs, TestItem item) => runConfig -> item -> Eff effs r

executeFileSystemInIO :: (a -> v) -> Eff '[FileSystem, Ensure, Error AppError, IO] a -> IO (Either AppError v)
executeFileSystemInIO func app = runM $ runError
                            $ ensureInterpreter
                            $ fileSystemIOInterpreter
                            $ func <$> app

executeFileSystemDocument :: (a -> b) -> Eff '[FileSystem, Ensure, Error AppError, Writer [String]] a -> (Either AppError b, [String])
executeFileSystemDocument func app = run
                              $ runWriter
                              $ runError
                              $ ensureInterpreter
                              $ interpret fileSystemDocInterpreter
                              $ func <$> app
