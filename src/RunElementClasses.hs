module RunElementClasses where

import           Pyrelude
import Data.Aeson.TH
import Language.Haskell.TH.Syntax
import Data.Aeson.Types
import           Check
import GHC.Records


-- this result is ultimately serialsed to JSON as part of the log protocol data  
data TestLogInfo = TestLogInfo {
  title :: Text,
  testConfig :: Value -- test Config as Json
}  deriving (Eq, Show)

instance Ord TestLogInfo where 
  (<=) v1 v2 = title v1 <= title v2

$(deriveJSON defaultOptions ''TestLogInfo)

data TestFilterResult = TestFilterResult {
  testInfo  :: TestLogInfo, 
  reasonForRejection :: Maybe Text
}  deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''TestFilterResult)

class HasField "title" a Text => Titled a

class (Titled a, Show a, FromJSON a, ToJSON a, Eq a) => Config a

class ItemClass i ds  where
  identifier :: i -> Int
  whenClause :: i -> Text
  thenClause :: i -> Text
  checkList :: i -> CheckDList ds

  whenThen :: i -> Text
  whenThen i = "When: " <> whenClause @i @ds i  <> "\n" <>
               "Then: " <> thenClause @i @ds i

mkDisplayInfo :: Config tc => tc -> TestLogInfo
mkDisplayInfo tc = TestLogInfo {
                      title = getField @"title" tc,
                      testConfig = toJSON tc
                    }
