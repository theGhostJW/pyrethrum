module List.Extra ( 
  module Data.List.Extra,
  List.Extra.last,
  List.Extra.init
) where 

--  shims for relude to ultimately be included in a revived pyrelude

import Data.List.Extra hiding (last, init)

-- other :: default - writefile is not text based
-- import Data.Text.IO (writeFile, putStrLn)

last:: [b] -> Maybe b
last = viaNonEmpty Prelude.last  

init :: [a] -> Maybe [a]
init = viaNonEmpty Prelude.init  