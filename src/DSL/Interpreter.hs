
module DSL.Interpreter where

import qualified Data.Aeson as A
import Common as C
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
import           Pyrelude as P hiding (app)

type EFFLogger effs = Member Logger effs
type ApEffs e effs = Members '[Logger e, Ensure, Error (AppError e)] effs

type EFFEnsureLog e effs = (Members '[Logger e, EP.Ensure] effs)
type EFFAllEffects e effs = Members (FullEffects e) effs
type FullEffects e = '[FileSystem, Ensure, ArbitraryIO, Logger e, CurrentTime, Error (AppError e)]

-- TODO is there a type level <> DRY out
type FullIOEffects e = '[FileSystem, EP.Ensure, ArbitraryIO, (Logger e), Reader ThreadInfo, State LogIndex, CurrentTime, Error (AppError e), Embed IO]
type TestIOEffects e = '[FileSystem, EP.Ensure, ArbitraryIO, Logger e, Reader ThreadInfo, State LogIndex, CurrentTime, Error (AppError e), Output (LogProtocol e), Embed IO]

type FullDocIOEffects e = '[FileSystem, EP.Ensure, ArbitraryIO, CurrentTime, Logger e, Reader ThreadInfo, State LogIndex, CurrentTime, Error (AppError e), Embed IO]
type FullDocEffects e = '[FileSystem, ArbitraryIO, Reader ThreadInfo, State LogIndex, CurrentTime, Logger e, Ensure, Error (AppError e), OutputDListText]

handleIOException :: IO (Either (AppError e) a) -> IO (Either (AppError e) a)
handleIOException = handle $ pure . Left . C.IOError

-- todo find a type level <> and replace cons with list
baseEffExecute :: forall effs a e. (Show e, A.ToJSON e, Member (Embed IO) effs) => (forall effs0. Members [CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs0 => Sem (Logger e ': effs0) a -> Sem effs0 a) ->  Sem (FileSystem ':  EP.Ensure ': ArbitraryIO ': Logger e ': Reader ThreadInfo ': State LogIndex ': CurrentTime ': Error (AppError e) ': effs) a -> Sem effs (Either (AppError e) a)
baseEffExecute logger app = runError
                              $ currentTimeIOInterpreter
                              $ evalState (LogIndex 0)
                              $ runThreadInfoReader
                              $ logger
                              $ arbitraryIOInterpreter
                              $ ensureInterpreter
                              $ fileSystemIOInterpreter
                              app

executeWithLogger :: forall a e. (Show e, A.ToJSON e) => (forall effs. Members [CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs => Sem (Logger e ': effs) a -> Sem effs a) -> Sem (FullIOEffects e) a -> IO (Either (AppError e) a)
executeWithLogger logger app = 
    handleIOException $ runM (baseEffExecute logger app)

executeInIOConsoleRaw :: forall a e. (Show e, A.ToJSON e) => Sem (FullIOEffects e) a -> IO (Either (AppError e) a)
executeInIOConsoleRaw = executeWithLogger logRunConsoleInterpreter
  
executeInIOConsolePretty :: forall a e. (Show e, A.ToJSON e) => Sem (FullIOEffects e) a -> IO (Either (AppError e) a)
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

executeForTest :: forall a e. (Show e, A.ToJSON e) => Sem (TestIOEffects e) a -> IO ([LogProtocol e], Either (AppError e) a)
executeForTest app = runM $ runOutputList 
                          $ runError
                          $ currentTimeIOInterpreter
                          $ evalState (LogIndex 0)
                          $ runThreadInfoReader
                          $ logRunRawInterpreter
                          $ arbitraryIOInterpreter
                          $ ensureInterpreter
                          $ fileSystemIOInterpreter
                          app
                           
documentWithLogger :: forall a e. (Show e, A.ToJSON e) => (forall effs. Members '[CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs => Sem (Logger e ': effs) a -> Sem effs a) -> Sem (FullDocIOEffects e) a -> IO (Either (AppError e) a)
documentWithLogger logger app = handleIOException
                                    $ runM
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

document :: forall a e. (Show e, A.ToJSON e) => (forall effs. Member OutputDListText effs => Sem (Logger e ': effs) a -> Sem effs a) -> Sem (FullDocEffects e) a -> (DList Text, Either (AppError e) a)
document logger app =   
    run .
    runOutputMonoid id $
      runError
      $ ensureDocInterpreter
      $ logger
      $ janFst2000UTCTimeInterpreter
      $ evalState (LogIndex 0)
      $ runThreadInfoReader
      $ arbitraryIODocInterpreter
      $ fileSystemDocInterpreter 
      app
      
documentRaw :: forall a e. (Show e, A.ToJSON e) => Sem (FullDocEffects e) a -> (DList Text, Either (AppError e) a)
documentRaw = document logDocInterpreter

documentPretty :: forall a e. (Show e, A.ToJSON e) => Sem (FullDocEffects e) a -> (DList Text, Either (AppError e) a)
documentPretty = document logDocPrettyInterpreter