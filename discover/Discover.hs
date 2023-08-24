module Discover where

import qualified WeederLibCopy.Weeder.Main as W
import qualified BasePrelude as P
import qualified WeederLibCopy.Weeder as W


-- $> Discover.discover
discover :: IO ()
discover = do 
   W.main
  -- (xcode, an) <- 
   W.discover
  -- print xcode