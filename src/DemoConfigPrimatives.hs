module DemoConfigPrimatives where

import           Data.Set            as S
import           Foundation.Extended

data Environment = TST | UAT | PreProd | Prod deriving (Show, Eq, Ord)
data Country = AU | NZ deriving (Show, Eq, Ord)
data Depth = DeepRegression | Regression | Connectivity | Special deriving (Show, Eq, Ord)
