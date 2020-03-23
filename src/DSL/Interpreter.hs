
module DSL.Interpreter where

import Common
import Polysemy 
import Polysemy.Output
import Polysemy.Error
import Polysemy.State
import Polysemy.Reader
import           DSL.FileSystem
import           DSL.Ensure as EP
import           DSL.Logger
import           DSL.ArbitraryIO
import           DSL.CurrentTime
import           DSL.CurrentTimeDocLogger
import           DSL.LogProtocol
import           Data.DList as D
import           Pyrelude as F hiding (app)

type EFFLogger effs = Member Logger effs
type ApEffs effs = Members '[Logger, Ensure, Error EnsureError, Error AppError] effs

type EFFEnsureLog effs = (Members '[Logger, EP.Ensure] effs)
type EFFAllEffects effs = Members FullEffects effs
type FullEffects = '[FileSystem, Ensure, ArbitraryIO, Logger, CurrentTime, Error EnsureError, Error AppError]
type FullIOEffects = '[FileSystem, EP.Ensure, ArbitraryIO, Logger, Reader ThreadInfo, State LogIndex, CurrentTime, Error FileSystemError, Error EnsureError, Error AppError, Embed IO]
type FullDocIOEffects = '[FileSystem, EP.Ensure, ArbitraryIO, CurrentTime, Logger, Reader ThreadInfo, State LogIndex, CurrentTime, Error FileSystemError, Error EnsureError, Error AppError, Embed IO]
type FullDocEffects = '[FileSystem, ArbitraryIO, Reader ThreadInfo, State LogIndex, CurrentTime, Logger, Ensure, Error EnsureError, Error AppError, DListOutput]

flattenErrors :: Either AppError (Either EnsureError (Either FileSystemError v)) -> Either AppError v
flattenErrors = 
  let
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
executeInIOConsoleRaw = executeWithLogger logConsoleInterpreter

executeInIOConsolePretty :: forall a. Sem FullIOEffects a -> IO (Either AppError a)
executeInIOConsolePretty = executeWithLogger logConsolePrettyInterpreter

executeWithLogger :: forall a. (forall effs. Members [CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs => Sem (Logger ': effs) a -> Sem effs a) -> Sem FullIOEffects a -> IO (Either AppError a)
executeWithLogger logger app = 
    handleIOException $ flattenErrors <$> runM
                                   ( 
                                     runError
                                      $ runError
                                      $ runError
                                      $ currentTimeIOInterpreter
                                      $ evalState (LogIndex 0)
                                      $ runThreadInfoReader
                                      $ logger
                                      $ arbitraryIOInterpreter
                                      $ ensureInterpreter
                                      $ fileSystemIOInterpreter
                                      app
                                  )

                              
documentWithLogger :: forall a. (forall effs. Members '[CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs => Sem (Logger ': effs) a -> Sem effs a) -> Sem FullDocIOEffects a -> IO (Either AppError a)
documentWithLogger logger app = handleIOException $ flattenErrors <$> runM
                                  (
                                    runError
                                    $ runError
                                    $ runError
                                    $ currentTimeIOInterpreter
                                    $ evalState (LogIndex 0)
                                    $ runThreadInfoReader
                                    $ logger
                                    $ currentTimeDocInterpreter
                                    $ arbitraryIODocInterpreter
                                    $ ensureDocInterpreter
                                    $ fileSystemDocInterpreter
                                    app
                                  )

-- todo come back to this nested errors drill down on reason
executeDocumentRaw :: forall a. Sem FullDocEffects a -> (DList Text, Either AppError a)
executeDocumentRaw = executeDocument logDocInterpreter

executeDocumentPretty :: forall a. Sem FullDocEffects a -> (DList Text, Either AppError a)
executeDocumentPretty = executeDocument logDocPrettyInterpreter

executeDocument :: forall a. (forall effs. Member DListOutput effs => Sem (Logger ': effs) a -> Sem effs a) -> Sem FullDocEffects a -> (DList Text, Either AppError a)
executeDocument logger app =   
    run .
    runOutputMonoid id $
    (mapLeft AppEnsureError =<<) <$>
    ( 
        runError 
      . runError
      . ensureInterpreter
      . logger
      . janFst2000UTCTimeInterpreter
      . evalState (LogIndex 0)
      . runThreadInfoReader
      . arbitraryIODocInterpreter
      . fileSystemDocInterpreter 
      $ app
    )