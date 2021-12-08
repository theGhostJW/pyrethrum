
module DSL.Interpreter where

import qualified Data.Aeson as A
import Common as C ( FrameworkError(IOError), OutputDListText )
import Polysemy ( Sem, Member, Embed, run, runM, Members ) 
import Polysemy.Output ( Output, runOutputList, runOutputMonoid, runOutputSem )
import Polysemy.Error as E ( Error, runError )
import Polysemy.State ( State, evalState )
import Polysemy.Reader ( Reader )
import DSL.FileSystem
    ( fileSystemDocInterpreter, fileSystemIOInterpreter, FileSystem )
import DSL.Logger
    ( Logger,
      logRunConsoleInterpreter,
      runThreadInfoReader,
      logConsolePrettyInterpreter,
      logRunRawInterpreter,
      logDocInterpreter,
      logDocPrettyInterpreter, consListLog )
import DSL.ArbitraryIO
    ( arbitraryIODocInterpreter, arbitraryIOInterpreter, ArbitraryIO )
import DSL.CurrentTime
    ( currentTimeIOInterpreter,
      janFst2000UTCTimeInterpreter,
      CurrentTime, janFst2000Midnight )
import DSL.CurrentTimeDocLogger ( currentTimeDocInterpreter )
import DSL.LogProtocol
    ( LogIndex(LogIndex), LogProtocolBase, ThreadInfo )
import Data.DList as D ( DList )
import Pyrelude as P
    ( ($),
      Show,
      Applicative(pure),
      IO,
      Either(Left),
      Text,
      Category((.), id),
      handle )


type Failure e = Error (FrameworkError e)
type MinEffs e effs = Members '[Logger e, Failure e, CurrentTime] effs

type EFFEnsureLog e effs = (Members '[Logger e, Failure e] effs)
type AllEffects e effs = Members (FullEffects e) effs
type FullEffects e = '[FileSystem, ArbitraryIO, Logger e, CurrentTime, Failure e]

-- TODO is there a type level <> DRY out
type FullIOMembersBase e = '[FileSystem, ArbitraryIO, Logger e, Reader ThreadInfo, State LogIndex, CurrentTime, Failure e, Embed IO]
type TestIOEffects e = '[FileSystem, ArbitraryIO, Logger e, Reader ThreadInfo, State LogIndex, CurrentTime, Failure e, Output (LogProtocolBase e), Embed IO]

type FullDocIOEffects e = '[FileSystem, ArbitraryIO, CurrentTime, Logger e, Reader ThreadInfo, State LogIndex, CurrentTime, Failure e, Embed IO]
type FullDocEffects e = '[FileSystem, ArbitraryIO, Reader ThreadInfo, State LogIndex, CurrentTime, Logger e, Failure e, OutputDListText]

handleIOException :: IO (Either (FrameworkError e) a) -> IO (Either (FrameworkError e) a)
handleIOException = handle $ pure . Left . C.IOError


-- todo find a type level <> and replace cons with list
baseEffExecute :: forall effs a e. (Show e, A.ToJSON e, Member (Embed IO) effs) => 
      (forall effs0. Members [CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs0 => Sem (Logger e ': effs0) a -> Sem effs0 a) 
      ->  Sem (FileSystem ': ArbitraryIO ': Logger e ': Reader ThreadInfo ': State LogIndex ': CurrentTime ': Failure e ': effs) a 
      -> Sem effs (Either (FrameworkError e) a)
baseEffExecute logger app = runError
                              $ currentTimeIOInterpreter
                              $ evalState (LogIndex 0)
                              $ runThreadInfoReader
                              $ logger
                              $ arbitraryIOInterpreter 
                              $ fileSystemIOInterpreter 
                              app

minInterpret  ::  forall r e. (Show e, A.ToJSON e) => Sem '[Logger e, Output (LogProtocolBase e), CurrentTime, Failure e] r -> Either (FrameworkError e) ([LogProtocolBase e], r)
minInterpret app = run 
                    $ runError
                    $ janFst2000UTCTimeInterpreter
                    $ runOutputList
                    $ consListLog
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