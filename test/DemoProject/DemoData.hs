{-# LANGUAGE QuasiQuotes #-}

module DemoProject.DemoData where

import           DemoProject.DemoRoughTest
import           Foundation.Extended

sampleItem =  Item {
  iid = 500,
  pre = "I do a test",
  post = "the test runs",
  path = [absfile|C:\Vids\SystemDesign\VidList.txt|],
  checks = mempty
}
