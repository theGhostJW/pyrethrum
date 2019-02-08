module ItemClass where

import           Check
import           Foundation.Extended
import           Foundation.List.DList
import           Data.Yaml
import GHC.Generics


class (ToJSON i, Generic i) => ItemClass i v | i -> v  where
  identifier :: i -> Int
  whenClause :: i -> String
  thenClause :: i -> String
  checkList :: i -> DList (Check v)

  whenThen :: i -> String
  whenThen i = "When: " <> whenClause i  <> "\n" <>
               "Then: " <> thenClause i
