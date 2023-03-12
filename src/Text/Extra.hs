module Text.Extra ( 
  last,
  init
) where 

--  shims for relude to ultimately be included in a revived pyrelude

import PyrethrumExtras
import qualified Data.Text as T 
import Prelude hiding (last, init)

-- other :: default - writefile is not text based
-- import Data.Text.IO (writeFile, putStrLn)


safeM :: (Monoid t, Eq t) => (t -> a) -> t -> Maybe a
safeM unsafeFunc t = t == mempty ? Nothing $ Just (unsafeFunc t) 
                                                                
last:: Text -> Maybe Char
last = safeM T.last  

init :: Text -> Maybe Text
init = safeM T.init