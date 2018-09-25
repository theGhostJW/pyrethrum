module TestAndRunConfig where

import           Foundation.Extended

class Titled a where
  title :: a -> String

type TestFilter runConfig testConfig =  runConfig -> testConfig -> Bool
