
module DSL.Interpreters where

import           Control.Monad.Freer
import           DSL.Ensure
import           DSL.FileSystem
import           Foundation.Extended

class TestItem a where
  identifier :: a -> Int
  whenClause :: a -> String
  thenClause :: a -> String
  whenThen :: a -> String
  whenThen a = "When: " <> whenClause a  <> "\n" <>
               "Then: " <> thenClause a

type InteractorFileSystem runConfig apState effs item = (Members '[Ensure, FileSystem] effs, TestItem item) => runConfig -> item -> Eff effs apState
