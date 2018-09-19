{-# LANGUAGE QuasiQuotes #-}

module DemoData where

import           DemoRoughTest
import           DSL.Interpreter
import           Foundation.Extended hiding (Item)
import qualified Prelude             as P

sampleItem =  Item {
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
