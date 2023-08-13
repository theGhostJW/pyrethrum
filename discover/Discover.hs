module Discover where

import qualified WeederLibCopy.Weeder.Main as W
import qualified BasePrelude as P
import qualified WeederLibCopy.Weeder as W


-- $ > Discover.discover
discover :: IO (P.ExitCode, W.Analysis)
discover = W.discover

