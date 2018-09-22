module TestConfig where

import           Foundation.Extended

class TestConfig a  where
  address :: a -> String
  title :: a -> String
