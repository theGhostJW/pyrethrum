
module DSL.Interpreter where

import DSL.Internal.Common
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DSL.FileSystem
import           DSL.Ensure
import           DSL.Logger
import           Foundation.List.DList
import           Foundation.Extended as F
import ItemClass
import Data.Either.Combinators
import Data.Tuple as T
import Control.Exception as E

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

handleIOException :: IO (Either AppError a) -> IO (Either AppError a)
handleIOException = E.handle $ pure . Left . IOError

type FullIOEffects = '[FileSystem, Logger, Ensure, Error FileSystemError, Error EnsureError, IO]

executeInIO :: forall a. Eff FullIOEffects a -> IO (Either AppError a)
executeInIO app = handleIOException $ unifyFSEnsureError <$> runM
                                 (
                                   runError
                                   $ runError
                                   $ ensureInterpreter
                                   $ logConsoleInterpreter
                                   $ fileSystemIOInterpreter
                                    app
                                 )

type FullDocEffects = '[FileSystem, Logger, Ensure, Error EnsureError, WriterDList]

executeDocument :: forall a. Eff FullDocEffects a -> Eff '[WriterDList] (Either AppError a)
executeDocument app =  (mapLeft AppEnsureError <$>) <$> runError
                                          $ ensureInterpreter
                                          $ logDocInterpreter
                                          $ fileSystemDocInterpreter app

extractDocLog :: Eff '[WriterDList] () -> DList String
extractDocLog app = F.snd $ run $ runWriter app
