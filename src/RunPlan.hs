
module RunPlan where

import Data.Aeson
import Common hiding (message)
import           Prelude as P hiding (First)
import           Control.Exception as E
import           DSL.Logger
import           DSL.LogProtocol
import Polysemy
import Polysemy.Error as PE
import RunElementClasses
import RunnerBase hiding (RunElement)
import qualified Data.Aeson as A

data HookFrequency = All | Each
data HookPosition = Before | After

-- data RunElement a where
--   Group :: Text -> i -> [RunElement a] -> RunElement a

--   Hook ::  {
--     action :: i -> Sem effs o,
--     body :: o -> RunElement effs v
--   } -> RunElement (Sem effs v) 

--   Tests ::  {
--         input :: i,
--         -- a list of tests
--         testList :: [i -> Sem effs ()]
--         -- eg [IO Either (FrameworkError TestInfo)]
--    } -> RunElement m1 m effs () 


