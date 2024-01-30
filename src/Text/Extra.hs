module Text.Extra ( 
  last,
  init,
  replaceFirst,
  module Data.Text
) where 

--  shims for relude to ultimately be included in a revived pyrelude

import Data.Text qualified as T 
import Data.Text hiding (last, init)
import Prelude hiding (last, init)
import PyrethrumExtras ( (?) )

safet :: (Text -> b) -> Text -> Maybe b
safet unsafef t = 
  T.null t ? Nothing $ Just (unsafef t)
                                                            
last:: Text -> Maybe Char
last = safet T.last  

init :: Text -> Maybe Text
init = safet T.init


--  https://stackoverflow.com/questions/14922070/haskell-use-data-text-replace-to-replace-only-the-first-occurrence-of-a-text-va
replaceFirst :: Text -- ^ needle
                  -> Text -- ^ replacement
                  -> Text -- ^ haystack
                  -> Text
replaceFirst needle replacement haystack
    | T.null back = haystack 
    | otherwise = T.concat [front, replacement, T.drop (T.length needle) back] 
      where
        (front, back) = breakOn needle haystack

-- TODO: hide putLnStr and friends and expose the same in Text.IO
