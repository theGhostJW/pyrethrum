module RunElementClasses
  ( AddressElemType(..),
    Address (..),
    AddressElem(..),
    AddressTxtElm(..),
    TestLogInfo (..),
    TestFilterResult (..),
    AddressedElm (..),
    Config,
    HasId,
    HasTitle,
    ItemClass,
    rootAddress,
    toStrElm,
    push,
    toTitleList,
    render,
    render',
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

data AddressElemType = Hook | Group | Test deriving (Show, Eq)

data AddressElem = AddressElem
  { title :: Text,
    elemType :: AddressElemType
  }
  deriving (Show, Eq)

newtype Address = Address {unAddress :: [AddressElem]} deriving (Show, Eq)

rootAddress :: Address
rootAddress = Address []

push :: Text -> AddressElemType -> Address -> Address
push t et add = Address $ AddressElem t et : unAddress add

toTitleList :: Address -> [Text]
toTitleList a = (title :: AddressElem -> Text) <$> reverse (unAddress a)

render :: Address -> Text
render = render' " > "

render' :: Text -> Address -> Text
render' delim add = intercalate delim $ reverse $ (title :: AddressElem -> Text) <$> unAddress add

instance Ord Address where
  v1 <= v2 = toTitleList v1 <= toTitleList v2

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
mkTestLogInfo a tc = TestLogInfo (getField @"title" tc) (push (getField @"title" tc) Test a) $ toJSON tc

data AddressedElm a = AddressedElm
  { address :: Address,
    element :: a
  }
  deriving (Show)
  
data AddressTxtElm a = AddressTxtElm
  { address :: Text,
    el :: a
  }
  deriving Show

toStrElm :: AddressedElm a -> AddressTxtElm a
toStrElm AddressedElm {address, element} = AddressTxtElm (render address) element

addressTitle :: AddressedElm a -> Text
addressTitle (AddressedElm (Address add) _) = P.headDef "" $ getField @"title" <$> add

instance Eq (AddressedElm a) where
  v1 == v2 = (address :: AddressedElm a -> Address) v1 == (address :: AddressedElm a -> Address) v2

instance Ord (AddressedElm a) where
  v1 <= v2 = (address :: AddressedElm a -> Address) v1 <= (address :: AddressedElm a -> Address) v2

$(deriveJSON defaultOptions ''AddressElemType)
$(deriveJSON defaultOptions ''AddressElem)
$(deriveJSON defaultOptions ''Address)
$(deriveJSON defaultOptions ''TestLogInfo)
$(deriveJSON defaultOptions ''TestFilterResult)