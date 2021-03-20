module RunElementClasses where

import           Pyrelude
import Data.Aeson.TH
import Language.Haskell.TH.Syntax
import Data.Aeson.Types
import           Check

newtype TestAddress = TestAddress {unTestAddress :: Text} deriving (Eq, Ord, Show, IsString)

-- this result is ultimately serialsed to JSON as part of the log protocol data  
-- type and can't serialise with custom typeclass constraints so forced to
-- have the redundant testModAddress and testTitle even though this
-- data is available via TestConfigClass
data TestDisplayInfo = TestDisplayInfo {
  testModAddress :: TestAddress,
  testTitle :: Text,
  testConfig :: Value -- test Config as Json
}  deriving (Eq, Show)

instance Ord TestDisplayInfo where 
  (<=) v1 v2 = testModAddress v1 <= testModAddress v2

$(deriveJSON defaultOptions ''TestDisplayInfo)

data TestFilterResult = TestFilterResult {
  testInfo  :: TestDisplayInfo, 
  reasonForRejection :: Maybe Text
}  deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''TestFilterResult)

mkTestAddress :: Name -> TestAddress
mkTestAddress = TestAddress . moduleOf

$(deriveJSON defaultOptions ''TestAddress)

toString :: TestAddress -> Text
toString (TestAddress s) = s

class Titled a where
  title :: a -> Text

class (Titled a, Show a, FromJSON a, ToJSON a, Eq a) => TestConfigClass a where
  moduleAddress:: a -> TestAddress

class (Titled a, Show a, FromJSON a, ToJSON a, Eq a) => RunConfigClass a

class (ToJSON i, Generic i) => ItemClass i ds  where
  identifier :: i -> Int
  whenClause :: i -> Text
  thenClause :: i -> Text
  checkList :: i -> CheckDList ds

  whenThen :: i -> Text
  whenThen i = "When: " <> whenClause @i @ds i  <> "\n" <>
               "Then: " <> thenClause @i @ds i

mkDisplayInfo :: TestConfigClass tc => tc -> TestDisplayInfo
mkDisplayInfo tc = TestDisplayInfo {
                                    testModAddress = moduleAddress tc,
                                    testTitle = title tc,
                                    testConfig = toJSON tc
                                  }
