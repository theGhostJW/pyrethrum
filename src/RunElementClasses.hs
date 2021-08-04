module RunElementClasses
  ( 
    Address,
    ItemClass(..),
    TestLogInfo (..),
    TestFilterResult (..),
    AddressedElm (..),
    HasId,
    Titled,
    Config,
    rootAddress,
    push,
    toList,
    render,
    mkTestLogInfo
  )
where

import Check
import Data.Aeson.TH
import Data.Aeson.Types
import GHC.Records
import Language.Haskell.TH.Syntax
import Pyrelude hiding (toList)
import Test.Tasty.Providers (IsTest)
import Test.Tasty.Runners (FailureReason (TestDepFailed))


newtype Address = Address { un :: [Text] } deriving (Show, Eq)

rootAddress :: Address
rootAddress = Address []

push :: Text -> Address -> Address
push t = Address . (t :) . un

toList :: Address -> [Text]
toList = reverse . un

render :: Text -> Address -> Text
render delim = intercalate delim . un

instance Ord Address where
   v1 <= v2 = RunElementClasses.toList v1 <= RunElementClasses.toList v2


-- this result is ultimately serialsed to JSON as part of the log protocol data
data TestLogInfo = TestLogInfo
  { title :: Text,
    address :: Address,
    testConfig :: Value -- test Config as Json
  }
  deriving (Eq, Show)


instance Ord TestLogInfo where
   v1 <= v2 = (address :: TestLogInfo -> Address) v1 <= (address :: TestLogInfo -> Address) v2

data TestFilterResult = TestFilterResult
  { testInfo :: TestLogInfo,
    reasonForRejection :: Maybe Text
  }
  deriving (Eq, Ord, Show)

class HasField "title" a Text => Titled a
class HasField "id" a Int => HasId a

class (Titled a, Show a, FromJSON a, ToJSON a, Eq a) => Config a

class HasId i => ItemClass i ds where
  whenClause :: i -> Text
  thenClause :: i -> Text
  checkList :: i -> CheckDList ds

  whenThen :: i -> Text
  whenThen i =
    "When: " <> whenClause @i @ds i <> "\n"
      <> "Then: "
      <> thenClause @i @ds i

mkTestLogInfo :: Config tc => Address -> tc -> TestLogInfo
mkTestLogInfo a tc = TestLogInfo (getField @"title" tc) (push (getField @"title" tc) a) $ toJSON tc


data AddressedElm a = AddressedElm
  { address :: Address,
    element :: a
  }
  deriving (Show)

instance Eq (AddressedElm a) where
  v1 == v2 = (address :: AddressedElm a -> Address) v1 == (address :: AddressedElm a -> Address) v2
instance Ord (AddressedElm a) where
   v1 <= v2 = (address :: AddressedElm a -> Address) v1 <= (address :: AddressedElm a -> Address) v2

$(deriveJSON defaultOptions ''Address)
$(deriveJSON defaultOptions ''TestLogInfo)
$(deriveJSON defaultOptions ''TestFilterResult)
