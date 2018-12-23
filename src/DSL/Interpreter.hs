
module DSL.Interpreter where

import DSL.Common
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DSL.FileSystem
import           DSL.Ensure
import           DSL.Logger
import           Foundation.List.DList
import           Foundation.Extended as F
import Data.Either.Combinators
import Control.Exception as E

type EFFLogger effs = Member Logger effs
type EFFEnsureLog effs = (Members '[Logger, Ensure] effs)
type EFFFileSystem effs = Members '[Logger, Ensure, FileSystem] effs

type EFFFileSystemInIO effs = (Members '[FileSystem, Ensure, Error FileSystemError, Error EnsureError, IO] effs)

unifyFSEnsureError :: Either EnsureError (Either FileSystemError v) -> Either AppError v
unifyFSEnsureError = \case
                       Right ee -> case ee of
                                       Right v -> Right v
                                       Left l -> Left $ AppFileSystemError l
                       Left enFail -> Left $ AppEnsureError enFail

handleIOException :: IO (Either AppError a) -> IO (Either AppError a)
handleIOException = E.handle $ pure . Left . IOError

type FullIOEffects = '[FileSystem, Logger, Ensure, Error FileSystemError, Error EnsureError, IO]

executeInIOConsoleRaw :: forall a. Eff FullIOEffects a -> IO (Either AppError a)
executeInIOConsoleRaw = executeInIO logConsoleInterpreter

executeInIOConsolePretty :: forall a. Eff FullIOEffects a -> IO (Either AppError a)
executeInIOConsolePretty = executeInIO logConsolePrettyInterpreter

executeInIO :: forall a. (forall effs. LastMember IO effs => Eff (Logger ': effs) ~> Eff effs) -> Eff FullIOEffects a -> IO (Either AppError a)
executeInIO logger app = handleIOException $ unifyFSEnsureError <$> runM
                                 (
                                   runError
                                   $ runError
                                   $ ensureInterpreter
                                   $ logger
                                   $ fileSystemIOInterpreter
                                    app
                                 )

type FullDocEffects = '[FileSystem, Logger, Ensure, Error EnsureError, WriterDList]

executeDocumentRaw :: forall a. Eff FullDocEffects a -> Eff '[WriterDList] (Either AppError a)
executeDocumentRaw = executeDocument logDocInterpreter

executeDocument :: forall a. (forall effs. Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs) -> Eff FullDocEffects a -> Eff '[WriterDList] (Either AppError a)
executeDocument logger app =  (mapLeft AppEnsureError <$>) <$> runError
                                          $ ensureInterpreter
                                          $ logger
                                          $ fileSystemDocInterpreter app

extractDocLog :: Eff '[WriterDList] () -> DList String
extractDocLog app = F.snd $ run $ runWriter app
