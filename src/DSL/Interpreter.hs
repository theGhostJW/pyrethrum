
module DSL.Interpreter where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DSL.FileSystem
import           DSL.Ensure
import           Foundation.Extended
import Data.Either.Combinators
import TestItem
import Check

type EFFFileSystem effs = (Members '[Ensure, FileSystem] effs)

data AppError =
              AppFileSystemError FileSystemError |
              AppEnsureError EnsureError |
              NotImplemented String |
              GenericError String |
              IOError IOException
              deriving (Show, Eq)

unifyFSEnsureError :: Either EnsureError (Either FileSystemError v) -> Either AppError v
unifyFSEnsureError = \case
                       Right ee -> case ee of
                                       Right v -> Right v
                                       Left l -> Left $ AppFileSystemError l
                       Left enFail -> Left $ AppEnsureError enFail

executeFileSystemInIO :: (a -> v) -> Eff '[FileSystem, Ensure, Error FileSystemError, Error EnsureError, IO] a -> IO (Either AppError v)
executeFileSystemInIO func app = unifyFSEnsureError <$> runM
                                  (
                                    runError
                                    $ runError
                                    $ ensureInterpreter
                                    $ fileSystemIOInterpreter
                                    $ func <$> app
                                  )

executeFileSystemDocument :: (a -> b) -> Eff '[FileSystem, Ensure, Error EnsureError, Writer [String]] a -> (Either AppError b, [String])
executeFileSystemDocument func app =  let (val, log) = run
                                                      $ runWriter
                                                      $ runError
                                                      $ ensureInterpreter
                                                      $ interpret fileSystemDocInterpreter
                                                      $ func <$> app
                                      in
                                        (mapLeft AppEnsureError val, log)
