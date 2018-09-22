{-# LANGUAGE QuasiQuotes #-}

module DemoData where

import           DemoRoughTest
import           Foundation.Extended

sampleItem =  TestItem {
  iid = 500,
  pre = "I do a test",
  post = "the test runs",
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|],
  checks = mempty
}

sampleRunConfig = RunConfig {
  environment = "Test",
  depth = 44,
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|]
}
