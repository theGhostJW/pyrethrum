
module DSL.CurrentTime where

import  DSL.LogProtocol
import DSL.LogProtocol.PrettyPrint
import           Data.DList
import           Pyrelude as P
import qualified Pyrelude.IO as PIO
import Text.Show.Pretty as PP
import RunElementClasses as C
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Polysemy.IO

data CurrentTime m a where
  GetTimeZone :: UTCTime -> CurrentTime m TimeZone
  GetCurrentTimeZone :: CurrentTime m TimeZone
  GetCurrentTime :: CurrentTime m UTCTime
  UtcToLocalZonedTime :: UTCTime -> CurrentTime m ZonedTime

makeSem ''CurrentTime

curretnTimeIOInterpreter :: Member (Embed IO) effs => Sem (CurrentTime ': effs) a -> Sem effs a
curretnTimeIOInterpreter = 
  interpret $ embed . \case 
                        GetTimeZone utcTime' -> PIO.getTimeZone utcTime'
                        GetCurrentTimeZone -> PIO.getCurrentTimeZone
                        GetCurrentTime -> PIO.getCurrentTime
                        UtcToLocalZonedTime utcTime' -> PIO.utcToLocalZonedTime utcTime'

                                 
  