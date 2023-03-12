module List.Extra ( 
  module Data.List.Extra,
  List.Extra.last,
  List.Extra.init,
  List.Extra.maximum
) where 

--  shims for relude to ultimately be included in a revived pyrelude

import Data.List.Extra as L
import Prelude hiding (last)
import Data.List.Extra hiding (last, init, maximum)


-- other :: default - writefile is not text based
-- import Data.Text.IO (writeFile, putStrLn)

safel :: ([a] -> b) -> [a] -> Maybe b
safel unsafef l = 
    l & \case 
      [] -> Nothing 
      _ -> Just $ unsafef l



last:: [b] -> Maybe b
last = safel L.last 

init :: [a] -> Maybe [a]
init = safel L.init  

maximum :: Ord a => [a] -> Maybe a
maximum = safel L.maximum  