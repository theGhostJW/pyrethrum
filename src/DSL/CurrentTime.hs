
module DSL.CurrentTime where

import           Pyrelude as P
import qualified Pyrelude.IO as PIO
import Polysemy
import Control.Lens

data CurrentTime m a where
  GetCurrentTime :: CurrentTime m UTCTime
  GetCurrentTimeZone :: CurrentTime m TimeZone
  GetTimeZone :: UTCTime -> CurrentTime m TimeZone
  UtcToLocalZonedTime :: UTCTime -> CurrentTime m ZonedTime

makeSem ''CurrentTime

currentTimeFromIO :: CurrentTime m a -> IO a
currentTimeFromIO = \case 
                        GetCurrentTime -> PIO.getCurrentTime
                        GetCurrentTimeZone -> PIO.getCurrentTimeZone
                        GetTimeZone utcTime' -> PIO.getTimeZone utcTime'
                        UtcToLocalZonedTime utcTime' -> PIO.utcToLocalZonedTime utcTime'

currentTimeIOInterpreter :: Member (Embed IO) effs => Sem (CurrentTime ': effs) a -> Sem effs a
currentTimeIOInterpreter = interpret $ embed . \case 
                                                  GetCurrentTime -> PIO.getCurrentTime
                                                  GetCurrentTimeZone -> PIO.getCurrentTimeZone
                                                  GetTimeZone utcTime' -> PIO.getTimeZone utcTime'
                                                  UtcToLocalZonedTime utcTime' -> PIO.utcToLocalZonedTime utcTime'

constTimeInterpreter :: forall effs a. UTCTime -> TimeZone -> TimeZone -> ZonedTime -> Sem (CurrentTime ': effs) a -> Sem effs a
constTimeInterpreter currentTime currentZone zoneFromTime localZonedTimeFromUTC = 
  interpret $ pure . \case 
                        GetCurrentTime -> currentTime
                        GetCurrentTimeZone -> currentZone 
                        GetTimeZone utcTime' -> zoneFromTime
                        UtcToLocalZonedTime utcTime' -> localZonedTimeFromUTC

janFst2000UTCTimeInterpreter :: Sem (CurrentTime ': effs) a -> Sem effs a
janFst2000UTCTimeInterpreter = constTimeInterpreter janFst2000Midnight utc utc ((utc, janFst2000Midnight) ^. zonedTime)

janFst2000Midnight :: UTCTime
janFst2000Midnight = UTCTime janFst2000 (timeOfDayToTime midnight) ^. from utcTime

janFst2000 :: Day
janFst2000 = fromGregorian 2000 1 1 

midNight :: TimeOfDay
midNight = TimeOfDay 0 0 0
