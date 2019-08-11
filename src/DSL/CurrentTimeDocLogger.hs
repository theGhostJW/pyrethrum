
module DSL.CurrentTimeDocLogger where

import           Pyrelude as P
import           Pyrelude.IO as PIO
import Polysemy
import Control.Lens
import DSL.CurrentTime as CT
import DSL.Logger

currentTimeDocInterpreter :: forall a effs. Members [Logger, Embed IO] effs => Sem (CurrentTime ': effs) a -> Sem effs a
currentTimeDocInterpreter = 
  interpret $ \ct ->
                  do
                    (lbl :: Text, v) <- embed $ case ct of
                            GetCurrentTime -> (\a -> ("getCurrentTime: " <> txt a, a)) <$> PIO.getCurrentTime
                            GetCurrentTimeZone -> (\a -> ("getCurrentTimeZone: " <> txt a, a)) <$> PIO.getCurrentTimeZone
                            GetTimeZone utcTime' -> (\a -> ("getTimeZone: " <> txt a, a)) <$> PIO.getTimeZone utcTime'
                            UtcToLocalZonedTime utcTime' -> (\a -> ("utcToLocalZonedTime: " <> txt a, a)) <$>PIO.utcToLocalZonedTime utcTime'
                    logDocAction lbl
                    pure v