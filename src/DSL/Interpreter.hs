
module DSL.Interpreter where

import DSL.Internal.Common
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DSL.FileSystem
import           DSL.Ensure
import           DSL.Logger
import           Foundation.List.DList
import           Foundation.Extended
import ItemClass
import Data.Either.Combinators
import Data.Tuple as T

type EFFLogger effs = Member Logger effs
type EFFEnsureLog effs = (Members '[Logger, Ensure] effs)
type EFFFileSystem effs = Members '[Logger, Ensure, FileSystem] effs

type EFFFileSystemInIO effs = (Members '[FileSystem, Ensure, Error FileSystemError, Error EnsureError, IO] effs)

data PreTestStage = Rollover |
                      GoHome
                      deriving (Show, Eq)

data AppError =
              AppFileSystemError FileSystemError |
              AppEnsureError EnsureError |

              NotImplementedError String |
              GenericError String |

              PreTestError PreTestStage String AppError |
              PreTestCheckExecutionError PreTestStage String AppError |
              PreTestCheckError PreTestStage String |

              IOError IOException

              deriving (Show, Eq)

executeFileSystemInIO :: forall a v. (a -> v) -> Eff '[FileSystem, Logger, Ensure, Error FileSystemError, Error EnsureError, IO] a -> IO (Either AppError v)
executeFileSystemInIO func app = unifyFSEnsureError <$> runM
                                  (
                                    runError
                                    $ runError
                                    $ ensureInterpreter
                                    $ logConsoleInterpreter
                                    $ fileSystemIOInterpreter
                                    $ func <$> app
                                  )

executeFileSystemDocument :: forall a b. (a -> b) -> Eff '[FileSystem, Logger, Ensure, Error EnsureError, WriterDList, IO] a -> IO (Either AppError b, DList String)
executeFileSystemDocument func app =  let
                                        vl :: IO (Either EnsureError b, DList String)
                                        vl = runM
                                              $ runWriter
                                              $ runError
                                              $ ensureInterpreter
                                              $ logConsoleInterpreter
                                              $ fileSystemDocInterpreter
                                              $ func <$> app

                                        mapError :: IO (Either EnsureError b, DList String) -> IO (Either AppError b, DList String)
                                        mapError r = do
                                                      (val, logs) <- r
                                                      pure (mapLeft AppEnsureError val, logs)

                                      in
                                        mapError vl


unifyFSEnsureError :: Either EnsureError (Either FileSystemError v) -> Either AppError v
unifyFSEnsureError = \case
                       Right ee -> case ee of
                                       Right v -> Right v
                                       Left l -> Left $ AppFileSystemError l
                       Left enFail -> Left $ AppEnsureError enFail


executeInIO :: forall a. Eff '[FileSystem,  Logger, Ensure, Error FileSystemError, Error EnsureError, IO] a -> IO (Either AppError a)
executeInIO app = unifyFSEnsureError <$> runM
                                 (
                                   runError
                                   $ runError
                                   $ ensureInterpreter
                                   $ logConsoleInterpreter
                                   $ fileSystemIOInterpreter
                                    app
                                 )

executeDocument :: forall a.  Eff '[FileSystem, Logger, Ensure, Error EnsureError, WriterDList, IO] a -> IO (Either AppError a)
executeDocument app =  let
                                        vl = runM
                                              $ runWriter
                                              $ runError
                                              $ ensureInterpreter
                                              $ logDocInterpreter
                                              $ fileSystemDocInterpreter app

                                        mapError :: IO (Either EnsureError b, DList String) -> IO (Either AppError b, DList String)
                                        mapError r = do
                                                      (val, logs) <- r
                                                      pure (mapLeft AppEnsureError val, logs)

                                      in
                                         T.fst <$> mapError vl
