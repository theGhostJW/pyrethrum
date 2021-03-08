
module DSL.CurrentTime where

import           Prelude as P
import Polysemy
import Chronos
import Data.Time

data CurrentTime m a where
  Now :: CurrentTime m Time
  GetTimeZone :: CurrentTime m TimeZone
  UtcOffset :: CurrentTime m Int

makeSem ''CurrentTime

currentTimeIOInterpreter :: Member (Embed IO) effs => Sem (CurrentTime ': effs) a -> Sem effs a
currentTimeIOInterpreter = undefined 


constTimeInterpreter :: forall effs a. Time -> TimeZone -> Sem (CurrentTime ': effs) a -> Sem effs a
constTimeInterpreter time zone  = 
  interpret $ pure . \case 
                        Now -> time
                        GetTimeZone -> zone
                        UtcOffset -> timeZoneMinutes zone

janFst2000Midnight :: Time
janFst2000Midnight = timeFromYmdhms 2000 0 1 0 0 0


janFst2000UTCTimeInterpreter :: Sem (CurrentTime ': effs) a -> Sem effs a
janFst2000UTCTimeInterpreter = constTimeInterpreter janFst2000Midnight utc

