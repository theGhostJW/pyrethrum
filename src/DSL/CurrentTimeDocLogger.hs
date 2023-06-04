
module DSL.CurrentTimeDocLogger where

import qualified Data.Aeson as A
import           Prelude as P
import           PyrethrumExtras.IO as PIO
import Polysemy
import DSL.CurrentTime as CT hiding (now)
import DSL.LoggerPsy
import DSL.LogProtocol
import PyrethrumExtras
import Chronos
import Data.Time (TimeZone(..))

currentTimeDocInterpreter :: forall a e effs. (Show e, A.ToJSON e, Members [Logger e, Embed IO] effs) => Sem (CurrentTime ': effs) a -> Sem effs a
currentTimeDocInterpreter = 
  interpret $ \ct ->
                  let 
                    showTup :: forall v. Show v => Text -> IO v -> IO (Text, v)
                    showTup lbl v = (\v' -> (lbl <> txt v', v')) <$> v
                  in
                    do
                      (lbl, v) <- embed $ case ct of
                              Now -> showTup "getCurrentTime: " now
                              GetTimeZone -> showTup "getTimeZone: " PIO.getCurrentTimeZone
                              UtcOffset -> showTup "utcOffset: " $ timeZoneMinutes <$> PIO.getCurrentTimeZone
                      logAction lbl
                      pure v