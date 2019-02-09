module TestAndRunConfig where

import           Foundation.Extended
import Data.Aeson.TH
import OrphanedInstances
import Language.Haskell.TH.Syntax
import Data.Aeson.Types

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