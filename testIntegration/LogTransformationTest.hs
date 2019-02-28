module LogTransformationTest where 

import           Foundation.Extended as E
import qualified Prelude             as P
import Test.Extended       as T
import AuxFiles
import Control.Monad
import LogTransformation

unit_demo :: IO ()
unit_demo = do 
                eFile <- jsoniFile 
                eItems <- P.traverse summariseIterations eFile
                chk $ isRight eItems  
                chk ("" /= fromRight' eItems)

jsoniFile :: IO (Either P.IOError AbsFile)
jsoniFile = dataFile [relfile|demo_raw_log.ijson|]