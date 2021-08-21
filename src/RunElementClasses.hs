module RunElementClasses
  (
    Address(..),
    TestLogInfo (..),
    TestFilterResult (..),
    AddressedElm (..),
    Config,
    HasId,
    HasTitle,
    ItemClass,
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
import Pyrelude as P hiding (toList)
import Test.Tasty.Providers (IsTest)
import Test.Tasty.Runners (FailureReason (TestDepFailed))


newtype Address = Address { unAddress :: [Text] } deriving (Show, Eq)

rootAddress :: Address
rootAddress = Address []

push :: Text -> Address -> Address
push t = Address . (t :) . unAddress

toList :: Address -> [Text]
toList = reverse . unAddress

render :: Address -> Text
render = render' " > "

render' :: Text -> Address -> Text
render' delim = intercalate delim . unAddress

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


type HasTitle a = HasField "title" a Text
type HasId a = HasField "id" a Int
class (HasField "title" a Text, Show a, FromJSON a, ToJSON a, Eq a) => Config a

type ItemClass i ds = (HasTitle i, HasId i, HasField "checks" i (Checks ds))

mkTestLogInfo :: Config tc => Address -> tc -> TestLogInfo
mkTestLogInfo a tc = TestLogInfo (getField @"title" tc) (push (getField @"title" tc) a) $ toJSON tc


data AddressedElm a = AddressedElm
  { address :: Address,
    element :: a
  }
  deriving (Show)

addressTitle :: AddressedElm a -> Text
addressTitle = P.headDef  "" . unAddress . (address :: AddressedElm a -> Address)


instance Eq (AddressedElm a) where
  v1 == v2 = (address :: AddressedElm a -> Address) v1 == (address :: AddressedElm a -> Address) v2
instance Ord (AddressedElm a) where
   v1 <= v2 = (address :: AddressedElm a -> Address) v1 <= (address :: AddressedElm a -> Address) v2

$(deriveJSON defaultOptions ''Address)
$(deriveJSON defaultOptions ''TestLogInfo)
$(deriveJSON defaultOptions ''TestFilterResult)
