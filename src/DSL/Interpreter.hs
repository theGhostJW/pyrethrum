
module DSL.Interpreter where

import Common
import Polysemy 
import Polysemy.Output
import Polysemy.Error
import           DSL.FileSystem
import           DSL.Ensure as EP
import           DSL.Logger
import           DSL.ArbitraryIO
import           DSL.CurrentTime
import           Data.DList as D
import           Pyrelude as F hiding (app)

type EFFLogger effs = Member Logger effs
type EFFEnsureLog effs = (Members '[Logger, EP.Ensure] effs)
type EFFAllEffects effs = Members FullEffects effs
type FullEffects = '[FileSystem, Ensure, ArbitraryIO, Logger, Error EnsureError]
type FullIOEffects = '[FileSystem, EP.Ensure, ArbitraryIO, Logger, CurrentTime, Error FileSystemError, Error EnsureError, Error AppError, Embed IO]
type FullDocEffects = '[FileSystem, ArbitraryIO, Logger, Ensure, Error EnsureError, WriterDList]

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

executeInIOConsoleRaw :: forall a. Sem FullIOEffects a -> IO (Either AppError a)
executeInIOConsoleRaw = executeInIO logConsoleInterpreter

executeInIOConsolePretty :: forall a. Sem FullIOEffects a -> IO (Either AppError a)
executeInIOConsolePretty = executeInIO logConsolePrettyInterpreter

executeInIO :: forall a. (forall effs. Members [CurrentTime, Embed IO ]effs => Sem (Logger ': effs) a -> Sem effs a) -> Sem FullIOEffects a -> IO (Either AppError a)
executeInIO logger app = handleIOException $ flattenErrors <$> 
                                 (
                                    runM
                                   . runError
                                   . runError
                                   . runError
                                   . currentTimeIOInterpreter
                                   . logger
                                   . arbitraryIOInterpreter
                                   . ensureInterpreter
                                   . fileSystemIOInterpreter
                                   $ app
                                 )

                              
documentInIO :: forall a. (forall effs. Sem (Logger ': effs) a -> Sem effs a) -> Sem FullIOEffects a -> IO (Either AppError a)
documentInIO logger app = handleIOException $ flattenErrors <$> 
                                 (
                                   runM
                                   . runError
                                   . runError
                                   . runError
                                   . currentTimeIOInterpreter
                                   . logger
                                   . arbitraryIODocInterpreter
                                   . ensureDocInterpreter
                                   . fileSystemDocInterpreter
                                   $ app
                                 )

executeDocumentRaw :: forall a. Sem FullDocEffects a -> Sem '[WriterDList] (Either AppError a)
executeDocumentRaw = executeDocument logDocInterpreter

executeDocumentPretty :: forall a. Sem FullDocEffects a -> Sem '[WriterDList] (Either AppError a)
executeDocumentPretty = executeDocument logDocPrettyInterpreter

executeDocument :: forall a. (forall effs. Member WriterDList effs => Sem (Logger ': effs) a -> Sem effs a) -> Sem FullDocEffects a -> Sem '[WriterDList] (Either AppError a)
executeDocument logger app =  (mapLeft AppEnsureError <$>) <$> runError
                                          $ ensureInterpreter
                                          $ logger
                                          $ arbitraryIODocInterpreter
                                          $ fileSystemDocInterpreter app

extractDocLog :: Sem '[WriterDList] () -> DList Text
extractDocLog app =  fst . run $ runOutputMonoid id app
