-- {-# LANGUAGE NoStrictData #-}

module ConcurrentSuiteSimple where

import Check
import qualified Check
import qualified Check as C
import DSL.ArbitraryIO
import DSL.CurrentTime
import DSL.FileSystem
import DSL.Interpreter (AllEffects, Failure, MinEffs)
import DSL.Logger as L
import Data.Aeson.TH
import Data.Aeson.Types hiding (One)
import GHC.Records (HasField (getField))
import ItemRunners (runItem)
import Polysemy
import Pyrelude as P
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

rcRunAll = RunConfig "Run Everything"

data RunConfig = RunConfig
  { title :: Text,
    dummy :: Bool
  }
  deriving (Eq, Show)

instance Config RunConfig

-- type DemoEffs effs = MinEffs Text effs
type DemoEffs effs = AllEffects Text effs

data TestConfig = TestConfig
  { title :: Text,
    dummy :: Bool
  }
  deriving (Show, Eq)

instance Config TestConfig

$(deriveJSON defaultOptions ''TestConfig)

-- | A standard test
type DemoTest hi i as ds effs = Test Text TestConfig RunConfig hi i as ds effs

data IntItem = IntItem
  { id :: Int,
    title :: Text,
    checks :: C.Checks Int
  }
  deriving (Show, Generic)

mkIntItem :: Int -> IntItem
mkIntItem i = IntItem i ("Int Test Id" <> txt i) $ chk "Always Pass" $ const True

mkTxtItem :: Int -> TextItem
mkTxtItem i = TextItem i ("Int Test Id" <> txt i) $ chk "Always Pass" $ const True

intItems :: Int -> p -> [IntItem]
intItems i rc = mkIntItem <$> take i [1 ..]

txtItems :: Int -> p -> [TextItem]
txtItems i rc = mkTxtItem <$> take i [1 ..]

data TextItem = TextItem
  { id :: Int,
    title :: Text,
    checks :: C.Checks Text
  }
  deriving (Show, Generic)

interact :: (HasId itm, Member (Logger e0) effs) => Text -> rc -> hi -> itm -> Sem effs hi
interact lgText rc hi itm = L.log (lgText <> " " <> txt (getField @"id" itm)) >> pure hi

instance ToJSON TextItem where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON IntItem where
  toEncoding = genericToEncoding defaultOptions

empti :: a -> [b]
empti = const ([] :: [b])

--                 as ->    rc     -> hi -> i -> Sem effs as
emptiInteractor :: as -> RunConfig -> hi -> i -> Sem effs as
emptiInteractor as _ _ _ = pure as

-- end demo

emptiParser :: ds -> as -> Sem effs ds
emptiParser ds _ = pure ds

---                           hi  itm     as   ds
test1WebTxt :: forall effs. Member (Logger Text) effs => DemoTest Text TextItem Text Text effs
test1WebTxt =
  Test
    { config =
        TestConfig
          { title = "test1WebTxt",
            dummy = True
          },
      items = txtItems 5,
      interactor = interact "test1WebTxt",
      parse = emptiParser "Blahh"
    }

test4WebTxt :: forall effs. Member (Logger Text) effs => DemoTest Text TextItem Text Text effs
test4WebTxt =
  Test
    { config =
        TestConfig
          { title = "test4WebTxt",
            dummy = True
          },
      items = txtItems 5,
      interactor = interact "test4WebTxt",
      parse = pure
    }

demoSuiteConcurrentSimple :: forall effs a. DemoEffs effs => SuiteSource Text TestConfig RunConfig effs a
demoSuiteConcurrentSimple runTest =
  R.TestSuite
    [ R.Group
        { title = "Group 1",
          gElms =
            [ OnceHook
                { title = "Group 1 >> Before 1",
                  bHook = \_ -> L.log "BH - Group 1 >> Before Hook 1" $> "hello",
                  hkElms =
                    [ R.Group
                        { title = "Group 1 >> Before 1 >> Group 1",
                          gElms =
                            [ Tests
                                \a hd ->
                                  [ runTest a hd test1WebTxt,
                                    -- no test items
                                    runTest a hd test4WebTxt
                                  ]
                            ]
                        }
                    ],
                  aHook = \_ -> L.log "AH - Group 1 >> After Hook 1"
                }
            ]
        }
    ]

concurrentRun :: forall effs. DemoEffs effs => Sem effs ()
concurrentRun =
  mkSem $
    RunParams
      { suite = demoSuiteConcurrentSimple,
        filters = [],
        itemIds = Nothing,
        itemRunner = runItem,
        rc =
          RunConfig
            { title = "Concurrent",
            dummy = True
            }
      }

$(deriveJSON defaultOptions ''RunConfig)
