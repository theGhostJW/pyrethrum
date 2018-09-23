module TestAndRunConfig where

import           Foundation.Extended

class Titled a where
  title :: a -> String

class Titled a => TestConfigClass a  where
  address :: a -> String

type TestFilter runConfig testConfig =  runConfig -> testConfig -> Bool
