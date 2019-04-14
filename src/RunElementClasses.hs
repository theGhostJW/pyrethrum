module RunElementClasses where

import           Pyrelude
import Data.Aeson.TH
import OrphanedInstances
import Language.Haskell.TH.Syntax
import Data.Aeson.Types
import GHC.Generics
import           Check

newtype TestModule = TestModule {unTestModule :: Text} deriving (Eq, Show, IsString)

-- this result is ultimately serialsed to JSON as part of the log protocol data  
-- type and can't serialise with custom typeclass constraints so forced to
-- have the redundant testModAddress and testTitle even though this
-- data is available via TestConfigClass
data TestDisplayInfo = TestDisplayInfo {
  testModAddress :: TestModule,
  testTitle :: Text,
  testConfig :: Value -- test Config as Json
}  deriving (Eq, Show)

$(deriveJSON defaultOptions ''TestDisplayInfo)

data FilterResult = FilterResult {
  testInfo  :: TestDisplayInfo, 
  reasonForRejection :: Maybe Text
}  deriving (Show, Eq)

$(deriveJSON defaultOptions ''FilterResult)

mkTestModule :: Name -> TestModule
mkTestModule = TestModule . moduleOf

$(deriveJSON defaultOptions ''TestModule)

toString :: TestModule -> Text
toString (TestModule s) = s

class Titled a where
  title :: a -> Text

class (Titled a, Show a, FromJSON a, ToJSON a, Eq a) => TestConfigClass a where
  moduleAddress:: a -> TestModule

class (Titled a, Show a, FromJSON a, ToJSON a, Eq a) => RunConfigClass a

class (ToJSON i, Generic i) => ItemClass i ds | i -> ds  where
  identifier :: i -> Int
  whenClause :: i -> Text
  thenClause :: i -> Text
  checkList :: i -> CheckDList ds

  whenThen :: i -> Text
  whenThen i = "When: " <> whenClause i  <> "\n" <>
               "Then: " <> thenClause i

mkDisplayInfo :: TestConfigClass tc => tc -> TestDisplayInfo
mkDisplayInfo tc = TestDisplayInfo {
                                    testModAddress = moduleAddress tc,
                                    testTitle = title tc,
                                    testConfig = toJSON tc
                                  }
