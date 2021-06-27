
module RunPlan where

import Data.Aeson
import Common hiding (message)
import           Pyrelude as P hiding (First)
import           Pyrelude.IO as PO
import           Control.Exception as E
import           DSL.Logger
import           DSL.LogProtocol
import Polysemy
import Polysemy.Error as PE
import RunElementClasses
import RunnerBase hiding (SuiteItem)
import qualified Data.Aeson as A

data HookFrequency = All | Each
data HookPosition = Before | After

-- data SuiteItem a where
--   Group :: Text -> i -> [SuiteItem a] -> SuiteItem a

--   Hook ::  {
--     action :: i -> Sem effs o,
--     body :: o -> SuiteItem effs v
--   } -> SuiteItem (Sem effs v) 

--   Tests ::  {
--         input :: i,
--         -- a list of tests
--         testList :: [i -> Sem effs ()]
--         -- eg [IO Either (FrameworkError TestInfo)]
--    } -> SuiteItem m1 m effs () 


