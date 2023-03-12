module Text.Extra ( 
  last,
  init
) where 

--  shims for relude to ultimately be included in a revived pyrelude

import qualified Data.Text as T 
import Prelude hiding (last, init)
import PyrethrumExtras ( (?) )

-- other :: default - writefile is not text based
-- import Data.Text.IO (writeFile, putStrLn)

safet :: (Text -> b) -> Text -> Maybe b
safet unsafef t = 
  T.null t ? Nothing $ Just $ unsafef t
                                                            
last:: Text -> Maybe Char
last = safet T.last  

init :: Text -> Maybe Text
init = safet T.init