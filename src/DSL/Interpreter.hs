
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

-- TODO is there a type level <> DRY out
type FullIOEffects = '[FileSystem, EP.Ensure, ArbitraryIO, Logger, Reader ThreadInfo, State LogIndex, CurrentTime, Error FileSystemError, Error EnsureError, Error AppError, Embed IO]
type TestIOEffects = '[FileSystem, EP.Ensure, ArbitraryIO, Logger, Reader ThreadInfo, State LogIndex, CurrentTime, Error FileSystemError, Error EnsureError, Error AppError, Output LogProtocol, Embed IO]

type FullDocIOEffects = '[FileSystem, EP.Ensure, ArbitraryIO, CurrentTime, Logger, Reader ThreadInfo, State LogIndex, CurrentTime, Error FileSystemError, Error EnsureError, Error AppError, Embed IO]
type FullDocEffects = '[FileSystem, ArbitraryIO, Reader ThreadInfo, State LogIndex, CurrentTime, Logger, Ensure, Error EnsureError, Error AppError, OutputDListText]

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

-- todo find a type level <> and replace cons with list
baseEffExecute :: forall effs a. Member (Embed IO) effs => (forall effs0. Members [CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs0 => Sem (Logger ': effs0) a -> Sem effs0 a) ->  Sem (FileSystem ':  EP.Ensure ': ArbitraryIO ': Logger ': Reader ThreadInfo ': State LogIndex ': CurrentTime ': Error FileSystemError ': Error EnsureError ': Error AppError ': effs) a -> Sem effs (Either AppError (Either EnsureError (Either FileSystemError a)))
baseEffExecute logger app = runError
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

executeWithLogger :: forall a. (forall effs. Members [CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs => Sem (Logger ': effs) a -> Sem effs a) -> Sem FullIOEffects a -> IO (Either AppError a)
executeWithLogger logger app = 
    handleIOException $ flattenErrors <$> runM (baseEffExecute logger app)

executeInIOConsoleRaw :: forall a. Sem FullIOEffects a -> IO (Either AppError a)
executeInIOConsoleRaw = executeWithLogger logRunConsoleInterpreter
  
executeInIOConsolePretty :: forall a. Sem FullIOEffects a -> IO (Either AppError a)
executeInIOConsolePretty = executeWithLogger logConsolePrettyInterpreter

-- todo find if this is possible
-- Could not deduce: Polysemy.Internal.Union.IndexOf
--                       effs0 (Polysemy.Internal.Union.Found effs0 (Output LogProtocol))
--                     ~ Output LogProtocol
--     arising from a use of `logRunRawInterpreter'
--   from the context: Members
--                       '[CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs0
-- executeForTest :: forall a. Sem TestIOEffects a -> IO ([LogProtocol], Either AppError a)
-- executeForTest app = second flattenErrors <$> runM (runOutputList $ baseEffExecute logRunRawInterpreter app)

executeForTest :: forall a. Sem TestIOEffects a -> IO ([LogProtocol], Either AppError a)
executeForTest app = second flattenErrors <$> runM (
                                                    runOutputList 
                                                    $ runError
                                                    $ runError
                                                    $ runError
                                                    $ currentTimeIOInterpreter
                                                    $ evalState (LogIndex 0)
                                                    $ runThreadInfoReader
                                                    $ logRunRawInterpreter
                                                    $ arbitraryIOInterpreter
                                                    $ ensureInterpreter
                                                    $ fileSystemIOInterpreter
                                                    app)
                           
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
documentRaw :: forall a. Sem FullDocEffects a -> (DList Text, Either AppError a)
documentRaw = document logDocInterpreter

documentPretty :: forall a. Sem FullDocEffects a -> (DList Text, Either AppError a)
documentPretty = document logDocPrettyInterpreter

document :: forall a. (forall effs. Member OutputDListText effs => Sem (Logger ': effs) a -> Sem effs a) -> Sem FullDocEffects a -> (DList Text, Either AppError a)
document logger app =   
    run .
    runOutputMonoid id $
    (mapLeft AppEnsureError =<<) <$>
    runError 
      (runError
      $ ensureDocInterpreter
      $ logger
      $ janFst2000UTCTimeInterpreter
      $ evalState (LogIndex 0)
      $ runThreadInfoReader
      $ arbitraryIODocInterpreter
      $ fileSystemDocInterpreter 
      app
    )