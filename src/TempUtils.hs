module TempUtils where

import Debug.Trace

-- functions that will be moved to pyrelude
debugLines :: Show a => [a] -> IO ()
debugLines = traverse_ (traceIO . show)