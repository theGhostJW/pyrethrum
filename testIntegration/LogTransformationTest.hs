module LogTransformationTest where 

import           Foundation.Extended
import qualified Prelude             as P
import Test.Extended       as T
import AuxFiles
import Control.Monad
import LogTransformation

-- unit_demo :: IO ()
-- unit_demo = _ 
--     -- jsoniFile >>= 
    --                   either 
    --                       (chkFail . show)
    --                       runLines

jsoniFile = dataFile [relfile|demo_raw_log.ijson|]