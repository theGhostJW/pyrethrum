
module DSL.CurrentTimeDocLogger where

import qualified Data.Aeson as A
import           Prelude as P
import Polysemy
import DSL.CurrentTime as CT
import DSL.Logger
import Data.Text

currentTimeDocInterpreter :: forall a e effs. (Show e, A.ToJSON e, Members [Logger e, Embed IO] effs) => Sem (CurrentTime ': effs) a -> Sem effs a
currentTimeDocInterpreter = undefined 