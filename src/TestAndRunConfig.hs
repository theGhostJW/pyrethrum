module TestAndRunConfig where

import           Foundation.Extended

class Titled a where
  title :: a -> String

class Titled a => TestConfigClass a where
  moduleAddress:: a -> String
