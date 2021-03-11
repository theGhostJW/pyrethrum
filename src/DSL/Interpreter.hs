
module DSL.Interpreter where

import qualified Data.Aeson as A
import Common as C
import Polysemy 
import Polysemy.Output
import Polysemy.Error as E
import Polysemy.State
import Polysemy.Reader
import           DSL.FileSystem
import           DSL.Logger
import           DSL.ArbitraryIO
import           DSL.CurrentTime
import           DSL.CurrentTimeDocLogger
import           DSL.LogProtocol
import           Data.DList as D
import           Pyrelude as P hiding (app)


type Failure e = Error (FrameworkError e)
type ApEffs e effs = Members '[Logger e, Failure e, CurrentTime] effs

type EFFEnsureLog e effs = (Members '[Logger e, Failure e] effs)
type EFFAllEffectsBase e effs = Members (FullEffects e) effs
type FullEffects e = '[FileSystem, ArbitraryIO, Logger e, CurrentTime, Failure e]

-- TODO is there a type level <> DRY out
type FullIOMembersBase e = '[FileSystem, ArbitraryIO, Logger e, Reader ThreadInfo, State LogIndex, CurrentTime, Failure e, Embed IO]
type TestIOEffects e = '[FileSystem, ArbitraryIO, Logger e, Reader ThreadInfo, State LogIndex, CurrentTime, Failure e, Output (LogProtocolBase e), Embed IO]

type FullDocIOEffects e = '[FileSystem, ArbitraryIO, CurrentTime, Logger e, Reader ThreadInfo, State LogIndex, CurrentTime, Failure e, Embed IO]
type FullDocEffects e = '[FileSystem, ArbitraryIO, Reader ThreadInfo, State LogIndex, CurrentTime, Logger e, Failure e, OutputDListText]

handleIOException :: IO (Either (FrameworkError e) a) -> IO (Either (FrameworkError e) a)
handleIOException = handle $ pure . Left . C.IOError

-- todo find a type level <> and replace cons with list
baseEffExecute :: forall effs a e. (Show e, A.ToJSON e, Member (Embed IO) effs) => (forall effs0. Members [CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs0 => Sem (Logger e ': effs0) a -> Sem effs0 a) ->  Sem (FileSystem ': ArbitraryIO ': Logger e ': Reader ThreadInfo ': State LogIndex ': CurrentTime ': Failure e ': effs) a -> Sem effs (Either (FrameworkError e) a)
baseEffExecute logger app = runError
                              $ currentTimeIOInterpreter
                              $ evalState (LogIndex 0)
                              $ runThreadInfoReader
                              $ logger
                              $ arbitraryIOInterpreter 
                              $ fileSystemIOInterpreter 
                              app

executeWithLogger :: forall a e. (Show e, A.ToJSON e) => (forall effs. Members [CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs => Sem (Logger e ': effs) a -> Sem effs a) -> Sem (FullIOMembersBase e) a -> IO (Either (FrameworkError e) a)
executeWithLogger logger app = 
    handleIOException $ runM (baseEffExecute logger app)

executeInIOConsoleRaw :: forall a e. (Show e, A.ToJSON e) => Sem (FullIOMembersBase e) a -> IO (Either (FrameworkError e) a)
executeInIOConsoleRaw = executeWithLogger logRunConsoleInterpreter
  
executeInIOConsolePretty :: forall a e. (Show e, A.ToJSON e) => Sem (FullIOMembersBase e) a -> IO (Either (FrameworkError e) a)
executeInIOConsolePretty = executeWithLogger logConsolePrettyInterpreter

executeForTest :: forall a e. (Show e, A.ToJSON e) => Sem (TestIOEffects e) a -> IO ([LogProtocolBase e], Either (FrameworkError e) a)
executeForTest app = runM $ runOutputList 
                          $ runError
                          $ currentTimeIOInterpreter
                          $ evalState (LogIndex 0)
                          $ runThreadInfoReader
                          $ logRunRawInterpreter
                          $ arbitraryIOInterpreter
                          $ fileSystemIOInterpreter
                          app
                           
documentWithLogger :: forall a e. (Show e, A.ToJSON e) => (forall effs. Members '[CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs => Sem (Logger e ': effs) a -> Sem effs a) -> Sem (FullDocIOEffects e) a -> IO (Either (FrameworkError e) a)
documentWithLogger logger app = handleIOException
                                    $ runM
                                    $ runError
                                    $ currentTimeIOInterpreter
                                    $ evalState (LogIndex 0)
                                    $ runThreadInfoReader
                                    $ logger
                                    $ currentTimeDocInterpreter
                                    $ arbitraryIODocInterpreter
                                    $ fileSystemDocInterpreter
                                    app

document :: forall a e. (Show e, A.ToJSON e) => (forall effs. Member OutputDListText effs => Sem (Logger e ': effs) a -> Sem effs a) -> Sem (FullDocEffects e) a -> (DList Text, Either (FrameworkError e) a)
document logger app =   
    run .
    runOutputMonoid id $
      runError
      $ logger
      $ janFst2000UTCTimeInterpreter
      $ evalState (LogIndex 0)
      $ runThreadInfoReader
      $ arbitraryIODocInterpreter
      $ fileSystemDocInterpreter 
      app
      
documentRaw :: forall a e. (Show e, A.ToJSON e) => Sem (FullDocEffects e) a -> (DList Text, Either (FrameworkError e) a)
documentRaw = document logDocInterpreter

documentPretty :: forall a e. (Show e, A.ToJSON e) => Sem (FullDocEffects e) a -> (DList Text, Either (FrameworkError e) a)
documentPretty = document logDocPrettyInterpreter