-- {-# LANGUAGE NoStrictData #-}

module ConcurrentSuiteTestOnly where

import Check
import qualified Check
import qualified Check as C
import DSL.ArbitraryIO
import DSL.CurrentTime
import DSL.FileSystemPsy
import DSL.Interpreter (AllEffects, Failure, MinEffs)
import DSL.LoggerPsy as L
import Data.Aeson.TH
import Data.Aeson.Types hiding (One)
import GHC.Records (HasField (getField))
import ItemRunners (runItem)
import Polysemy
import Runner as R
  ( Address,
    Config,
    HasId,
    ItemClass,
    RunParams (..),
    SuiteItem (..),
    SuiteSource,
    Test (..),
    TestSuite (..),
    mkSem,
  )
import PyrethrumExtras

data RunConfig = RunConfig
  { title :: Text,
    dummy :: Bool
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''RunConfig)

instance Config RunConfig

-- type DemoEffs effs = MinEffs Text effs
type DemoEffs effs = AllEffects Text effs

data TestConfig = TestConfig
  { title :: Text,
    dummy :: Bool
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestConfig)

instance Config TestConfig


-- | A standard test
type DemoTest hi i as ds effs = Test Text TestConfig RunConfig hi i as ds effs

mkTxtItem :: Int -> TextItem
mkTxtItem i = TextItem i ("Int Test Id" <> txt i) $ chk "Always Pass" $ const True


txtItems :: Int -> p -> [TextItem]
txtItems i rc = mkTxtItem <$> take i [1 ..]

data TextItem = TextItem
  { id :: Int,
    title :: Text,
    checks :: C.Checks Text
  }
  deriving (Show, Generic)


interactConst :: (HasId itm, Member (Logger e0) effs) => Text -> a -> rc -> hi -> itm -> Sem effs a
interactConst lgText a rc _hi itm = L.log (lgText <> " " <> txt (getField @"id" itm)) >> pure a

instance ToJSON TextItem where
  toEncoding = genericToEncoding defaultOptions


-- end demo

emptiParser :: ds -> as -> Sem effs ds
emptiParser ds _ = pure ds

---                           hi  itm     as   ds
testVoidIn1 :: forall effs. Member (Logger Text) effs => DemoTest () TextItem Text Text effs
testVoidIn1 =
  Test
    { config =
        TestConfig
          { title = "testVoidIn1",
            dummy = True
          },
      items = txtItems 5,
      interactor = interactConst "testVoidIn1" "testVoidIn1 result",
      parse = emptiParser "Blahh"
    }

testVoidIn2 :: forall effs. Member (Logger Text) effs => DemoTest () TextItem Text Text effs
testVoidIn2 =
  Test
    { config =
        TestConfig
          { title = "testVoidIn2",
            dummy = True
          },
      items = txtItems 5,
      interactor = interactConst "testVoidIn2" "testVoidIn2 Result",
      parse = pure
    }


demoSuiteConcurrentTestsOnly :: forall effs a. DemoEffs effs => SuiteSource Text TestConfig RunConfig effs a
demoSuiteConcurrentTestsOnly runTest =
  R.TestSuite
    [ Tests
        \a hd ->
          [ runTest a hd testVoidIn1,
            runTest a hd testVoidIn2
          ]
    ]

concurrentTestOnlyPrms :: forall effs. DemoEffs effs => RunParams Maybe Text RunConfig TestConfig effs
concurrentTestOnlyPrms =
  RunParams
    { suite = demoSuiteConcurrentTestsOnly,
      filters = [],
      itemIds = Nothing,
      itemRunner = runItem,
      rc =
        RunConfig
          { title = "Concurrent Test Only",
            dummy = True
          }
    }