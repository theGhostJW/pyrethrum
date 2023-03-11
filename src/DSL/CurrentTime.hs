
module DSL.CurrentTime where

import           PyrethrumExtras as P
import qualified PyrethrumExtras.IO as PIO
import Polysemy
import Chronos as C
import Data.Time (getCurrentTimeZone, TimeZone(..), utc)


data CurrentTime m a where
  Now :: CurrentTime m Time
  GetTimeZone :: CurrentTime m TimeZone
  UtcOffset :: CurrentTime m Int

makeSem ''CurrentTime

currentTimeIOInterpreter :: Member (Embed IO) effs => Sem (CurrentTime ': effs) a -> Sem effs a
currentTimeIOInterpreter = interpret $ embed . \case 
                                                  Now -> C.now
                                                  GetTimeZone -> getCurrentTimeZone
                                                  UtcOffset -> timeZoneMinutes <$> getCurrentTimeZone


constTimeInterpreter :: forall effs a. Time -> TimeZone -> Sem (CurrentTime ': effs) a -> Sem effs a
constTimeInterpreter time zone  = 
  interpret $ pure . \case 
                        Now -> time
                        GetTimeZone -> zone
                        UtcOffset -> timeZoneMinutes zone

janFst2000Midnight :: Time
janFst2000Midnight = timeFromYmdhms 2000 0 1 0 0 0

midNight :: TimeOfDay
midNight = TimeOfDay 0 0 0

janFst2000UTCTimeInterpreter :: Sem (CurrentTime ': effs) a -> Sem effs a
janFst2000UTCTimeInterpreter = constTimeInterpreter janFst2000Midnight utc

