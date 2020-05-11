
module DSL.CurrentTime where

import           Pyrelude as P
import qualified Pyrelude.IO as PIO
import Polysemy
import Control.Lens

data CurrentTime m a where
  Now :: CurrentTime m Time
  GetTimeZone :: CurrentTime m TimeZone

makeSem ''CurrentTime

currentTimeIOInterpreter :: Member (Embed IO) effs => Sem (CurrentTime ': effs) a -> Sem effs a
currentTimeIOInterpreter = interpret $ embed . \case 
                                                  Now -> PIO.now
                                                  GetTimeZone -> PIO.getCurrentTimeZone


constTimeInterpreter :: forall effs a. Time -> TimeZone -> Sem (CurrentTime ': effs) a -> Sem effs a
constTimeInterpreter time zone  = 
  interpret $ pure . \case 
                        Now -> time
                        GetTimeZone -> zone

janFst2000Midnight :: Time
janFst2000Midnight = timeFromYmdhms 2000 0 1 0 0 0

midNight :: TimeOfDay
midNight = TimeOfDay 0 0 0

janFst2000UTCTimeInterpreter :: Sem (CurrentTime ': effs) a -> Sem effs a
janFst2000UTCTimeInterpreter = constTimeInterpreter janFst2000Midnight utc

