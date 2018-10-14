
module DSL.Interpreter where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DSL.FileSystem
import           DSL.Ensure
import           DSL.Logger
import           Foundation.Extended
import ItemClass
import Data.Either.Combinators

type EFFLogger effs = Member Logger effs
type EFFEnsureOnly effs = (Members '[Logger, Ensure] effs)
type EFFFileSystem effs = (Members '[Logger, Ensure, FileSystem] effs)

type EFFFileSystemInIO effs = (Members '[FileSystem, Ensure, Error FileSystemError, Error EnsureError, IO] effs)

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

executeFileSystemInIO :: forall a v. (a -> v) -> Eff '[FileSystem,  Logger, Ensure, Error FileSystemError, Error EnsureError, IO] a -> IO (Either AppError v)
executeFileSystemInIO func app = unifyFSEnsureError <$> runM
                                  (
                                    runError
                                    $ runError
                                    $ ensureInterpreter
                                    $ logConsoleInterpreter
                                    $ fileSystemIOInterpreter
                                    $ func <$> app
                                  )

executeFileSystemDocument :: forall a b. (a -> b) -> Eff '[FileSystem, Logger, Ensure, Error EnsureError, Writer [String], IO] a -> IO (Either AppError b, [String])
executeFileSystemDocument func app =  let
                                        vl :: IO (Either EnsureError b, [String])
                                        vl = runM
                                              $ runWriter
                                              $ runError
                                              $ ensureInterpreter
                                              $ logConsoleInterpreter
                                              $ fileSystemDocInterpreter
                                              $ func <$> app

                                        mapError :: IO (Either EnsureError b, [String]) -> IO (Either AppError b, [String])
                                        mapError r = do
                                                      (val, logs) <- r
                                                      pure (mapLeft AppEnsureError val, logs)

                                      in
                                        mapError vl
