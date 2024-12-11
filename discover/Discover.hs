module Discover where

import WeederLibCopy.Weeder.Main qualified as W
import BasePrelude qualified as P
import WeederLibCopy.Weeder qualified as W


{-
- list modules ending in Test
- or Hook
- extract Fixtures and depended on hooks
- extract hook dependencies 
- validate??
- generate main
-}


-- $> discover
discover :: IO ()
discover = do
  putStrLn "Discovering..."
  P.withArgs [] W.main
