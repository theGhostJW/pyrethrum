
module DSL.Interpreter where

import DSL.Common
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           DSL.FileSystem
import           DSL.Ensure
import           DSL.Logger
import           DSL.ArbitraryIO
import           Foundation.List.DList
import           Foundation.Extended as F
import AuxFiles
import qualified System.IO as S
import qualified Prelude as P


type EFFLogger effs = Member Logger effs
type EFFEnsureLog effs = (Members '[Logger, Ensure] effs)
type EFFAllEffects effs = Members FullEffects effs

flattenErrors :: Either AppError (Either EnsureError (Either FileSystemError v)) -> Either AppError v
flattenErrors = let
                        flattenInner :: Either EnsureError (Either FileSystemError v) -> Either AppError v
                        flattenInner = \case
                                           Right ee -> case ee of
                                                           Right v -> Right v
                                                           Left l -> Left $ AppFileSystemError l
                                           Left enFail -> Left $ AppEnsureError enFail
                     in
                        \case
                            Right etFsEr -> flattenInner etFsEr
                            Left apErr -> Left apErr

handleIOException :: IO (Either AppError a) -> IO (Either AppError a)
handleIOException = handle $ pure . Left . AppIOError

executeInIOConsoleRaw :: forall a. Eff FullIOEffects a -> IO (Either AppError a)
executeInIOConsoleRaw = executeInIO logConsoleInterpreter

executeInIOConsolePretty :: forall a. Eff FullIOEffects a -> IO (Either AppError a)
executeInIOConsolePretty = executeInIO logConsolePrettyInterpreter

type FullIOEffects = '[FileSystem, Ensure, ArbitraryIO, Logger, Error FileSystemError, Error EnsureError, Error AppError, IO]
type FullEffects = '[FileSystem, Ensure, ArbitraryIO, Logger, Error EnsureError]

executeInIO :: forall a. (forall effs. LastMember IO effs => Eff (Logger ': effs) ~> Eff effs) -> Eff FullIOEffects a -> IO (Either AppError a)
executeInIO logger app = handleIOException $ flattenErrors <$> runM
                                 (
                                   runError
                                   $ runError
                                   $ runError
                                   $ logger
                                   $ arbitraryIOIOInterpreter
                                   $ ensureInterpreter
                                   $ fileSystemIOInterpreter
                                    app
                                 )

type FullDocEffects = '[FileSystem, ArbitraryIO, Logger, Ensure, Error EnsureError, WriterDList]

executeDocumentRaw :: forall a. Eff FullDocEffects a -> Eff '[WriterDList] (Either AppError a)
executeDocumentRaw = executeDocument logDocInterpreter

executeDocumentPretty :: forall a. Eff FullDocEffects a -> Eff '[WriterDList] (Either AppError a)
executeDocumentPretty = executeDocument logDocPrettyInterpreter

executeDocument :: forall a. (forall effs. Member WriterDList effs => Eff (Logger ': effs) ~> Eff effs) -> Eff FullDocEffects a -> Eff '[WriterDList] (Either AppError a)
executeDocument logger app =  (mapLeft AppEnsureError <$>) <$> runError
                                          $ ensureInterpreter
                                          $ logger
                                          $ arbitraryIODocInterpreter
                                          $ fileSystemDocInterpreter app

extractDocLog :: Eff '[WriterDList] () -> DList String
extractDocLog app = F.snd $ run $ runWriter app
