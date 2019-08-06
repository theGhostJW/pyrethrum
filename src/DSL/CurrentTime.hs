
module DSL.CurrentTime where

import           Pyrelude as P
import qualified Pyrelude.IO as PIO
import Polysemy

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