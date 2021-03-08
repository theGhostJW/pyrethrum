
module DSL.Interpreter where

import qualified Data.Aeson as A
import Common as C
import Polysemy 
import Polysemy.Output
import Polysemy.Error
import           DSL.Ensure as EP
import           DSL.Logger
import           DSL.CurrentTime
import           DSL.CurrentTimeDocLogger
import           DSL.LogProtocol
import           Data.DList as D
import           Prelude as P hiding (app)

type ApEffs e effs = Members '[Logger e, Ensure, Error (FrameworkError e), CurrentTime] effs

type EFFEnsureLog e effs = (Members '[Logger e, EP.Ensure] effs)
type EFFAllEffectsBase e effs = Members (FullEffects e) effs
type FullEffects e = '[ Ensure, Logger e, CurrentTime, Error (FrameworkError e)]


handleIOException :: IO (Either (FrameworkError e) a) -> IO (Either (FrameworkError e) a)
handleIOException = undefined

-- todo find if this is possible
-- Could not deduce: Polysemy.Internal.Union.IndexOf
--                       effs0 (Polysemy.Internal.Union.Found effs0 (Output LogProtocolBase))
--                     ~ Output LogProtocolBase
--     arising from a use of `logRunRawInterpreter'
--   from the context: Members
--                       '[CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs0
-- executeForTest :: forall a. Sem TestIOEffects a -> IO ([LogProtocolBase], Either FrameworkError a)
-- executeForTest app = second flattenErrors <$> runM (runOutputList $ baseEffExecute logRunRawInterpreter app)
