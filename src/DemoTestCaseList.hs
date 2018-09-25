module DemoTestCaseList where

import           DemoConfig
import           DemoRoughTest
import           DemoRoughTestSimple
import           Foundation.Extended
import           Runner

testRun :: [String]
testRun = [
  --runFullTestShow DemoRoughTest.test --,
  --runFullTestShow DemoRoughTestSimple.test
  ]
