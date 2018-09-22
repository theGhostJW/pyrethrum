{-# LANGUAGE QuasiQuotes #-}

module DemoConfig where

import           Foundation.Extended

data RunConfig = RunConfig {
  environment :: String,
  depth       :: Integer,
  path        :: Path Abs File
}

sampleRunConfig = RunConfig {
  environment = "Test",
  depth = 44,
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|]
}
