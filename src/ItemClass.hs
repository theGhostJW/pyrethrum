module ItemClass where

import           Check
import           Foundation.Extended
import           Foundation.List.DList


class ItemClass a v | a -> v where
  identifier :: a -> Int
  whenClause :: a -> String
  thenClause :: a -> String
  checkList :: a -> DList (Check v)
  
  whenThen :: a -> String
  whenThen a = "When: " <> whenClause a  <> "\n" <>
               "Then: " <> thenClause a
