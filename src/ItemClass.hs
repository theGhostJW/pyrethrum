module ItemClass where

import           Check
import           Foundation.Extended
import           Foundation.List.DList


class ItemClass i v | i -> v where
  identifier :: i -> Int
  whenClause :: i -> String
  thenClause :: i -> String
  checkList :: i -> DList (Check v)

  whenThen :: i -> String
  whenThen i = "When: " <> whenClause i  <> "\n" <>
               "Then: " <> thenClause i
