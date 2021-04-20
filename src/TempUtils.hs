module TempUtils where

import Pyrelude
import Debug.Trace

-- functions that will be moved to pyrelude
debugLines :: [Text] -> IO ()
debugLines = traverse_ (traceIO . toS)