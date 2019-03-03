module RunElementClasses where

import           Foundation.Extended
import Data.Aeson.TH
import OrphanedInstances
import Language.Haskell.TH.Syntax
import Data.Aeson.Types
import           Foundation.List.DList
import GHC.Generics
import           Check

newtype TestModule = TestModule String deriving (Eq, Show, IsString)

mkTestModule :: Name -> TestModule
mkTestModule = TestModule . moduleOf

$(deriveJSON defaultOptions ''TestModule)

toString :: TestModule -> String
toString (TestModule s) = s

class Titled a where
  title :: a -> String

class (Titled a, Show a, FromJSON a, ToJSON a, Eq a) => TestConfigClass a where
  moduleAddress:: a -> TestModule

class (Titled a, Show a, FromJSON a, ToJSON a, Eq a) => RunConfigClass a

class (ToJSON i, Generic i) => ItemClass i ds | i -> ds  where
  identifier :: i -> Int
  whenClause :: i -> String
  thenClause :: i -> String
  checkList :: i -> CheckList ds

  whenThen :: i -> String
  whenThen i = "When: " <> whenClause i  <> "\n" <>
               "Then: " <> thenClause i
