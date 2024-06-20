module Discover where

import WeederLibCopy.Weeder.Main qualified as W
import BasePrelude qualified as P
import WeederLibCopy.Weeder qualified as W

-- $ > Discover.discover
discover :: IO ()
discover = do 
  --- W.main
  -- (xcode, an) <- 
  W.discover
  -- print xcode