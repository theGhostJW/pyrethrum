module TestItem where

import           Check
import           Foundation.Extended
import           Foundation.List.DList


class TestItem a v | a -> v where
  identifier :: a -> Int
  whenClause :: a -> String
  thenClause :: a -> String
  whenThen :: a -> String
  whenThen a = "When: " <> whenClause a  <> "\n" <>
               "Then: " <> thenClause a
  checkList :: a -> DList (Check v)
