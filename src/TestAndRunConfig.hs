module TestAndRunConfig where

import           Foundation.Extended
import Data.Aeson.TH
import OrphanedInstances
import Language.Haskell.TH.Syntax

newtype TestModule = TestModule String deriving (Eq, Show, IsString)

mkTestModule :: Name -> TestModule
mkTestModule = TestModule . moduleOf

$(deriveJSON defaultOptions ''TestModule)

toString :: TestModule -> String
toString (TestModule s) = s

class Titled a where
  title :: a -> String

class Titled a => TestConfigClass a where
  moduleAddress:: a -> TestModule
