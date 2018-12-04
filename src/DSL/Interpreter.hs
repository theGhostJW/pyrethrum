
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

executeDocument :: forall a.  Eff '[FileSystem, Logger, Ensure, Error EnsureError, WriterDList] a -> (Either AppError a, DList String)
executeDocument app =  let
                            -- (Either EnsureError a, DList String)
                            (val, logs) = run
                                          $ runWriter
                                          $ runError
                                          $ ensureInterpreter
                                          $ logDocInterpreter
                                          $ fileSystemDocInterpreter app
                            in
                             (mapLeft AppEnsureError val, logs)
