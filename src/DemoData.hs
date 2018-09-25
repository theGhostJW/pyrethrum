{-# LANGUAGE QuasiQuotes #-}

module DemoData where

import           DemoRoughTest
import           Foundation.Extended

sampleItem =  Item {
  iid = 500,
  pre = "I do a test",
  post = "the test runs",
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|],
  checks = mempty
}
