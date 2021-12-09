-- {-# LANGUAGE NoStrictData #-}

module MockSuite where

-- import Pyrelude.Test hiding (Group, maybe)
-- import Pyrelude.Test hiding (Group, maybe)
-- import Pyrelude.Test hiding (Group, maybe)
-- import Pyrelude.Test hiding (Group, maybe)

import qualified Check
import qualified Check as C
import DSL.ArbitraryIO
import DSL.CurrentTime
import DSL.FileSystem
import DSL.Interpreter (AllEffects, Failure, MinEffs)
import DSL.Logger as L
import Data.Aeson.TH
import Data.Aeson.Types hiding (One)
import Data.Yaml
import GHC.Records (HasField)
import ItemRunners (runItem)
import Polysemy
import Pyrelude as P
import Runner as R
  ( Address,
    Config,
    ItemClass,
    RunParams (..),
    SuiteItem (..),
    SuiteSource,
    Test (..),
    TestSuite (..),
    mkSem,
  )
import TestFilter
import Check

data Channel = Web | REST deriving (Eq, Ord, Show)

data ChannelSelect = WebOnly | RESTOnly | AllChannels deriving (Eq, Ord, Show)

rcRunAll = RunConfig "Run Everything" AllChannels

data RunConfig = RunConfig
  { title :: Text,
    target :: ChannelSelect
  }
  deriving (Eq, Show)

instance Config RunConfig

-- type DemoEffs effs = MinEffs Text effs
type DemoEffs effs = AllEffects Text effs

data TestConfig = TestConfig
  { title :: Text,
    channel :: Channel
  }
  deriving (Show, Eq)

instance Config TestConfig

$(deriveJSON defaultOptions ''TestConfig)

-- | A standard test
type MockTest hi i as ds effs = Test Text TestConfig RunConfig hi i as ds effs

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

intItems i rc = mkIntItem <$> take i [1..]

txtItems i rc = mkTxtItem <$> take i [1..]

data TextItem = TextItem
  { id :: Int,
    title :: Text,
    checks :: C.Checks Text
  }
  deriving (Show, Generic)

instance ToJSON TextItem where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON IntItem where
  toEncoding = genericToEncoding defaultOptions

empti :: a -> [b]
empti = const ([] :: [b])

--                 as ->    rc     -> hi -> i -> Sem effs as
emptiInteractor :: as -> RunConfig -> hi -> i -> Sem effs as
emptiInteractor as _ _ _ = pure as

beforAll :: Int -> Sem effs Text
beforAll = uu

-- demo manual beforeAll hooks
testInteractor :: RunConfig -> Text -> i -> Sem effs as
testInteractor _ _ _ = uu

implementedInteractor :: RunConfig -> Int -> i -> Sem effs as
implementedInteractor rc int' i = beforAll int' >>= \t -> testInteractor rc t i

-- end demo

emptiParser :: ds -> as -> Sem effs ds
emptiParser ds _ = pure ds

---                           hi  itm     as   ds
test1WebTxt :: forall effs. Member (Logger Text) effs => MockTest Text TextItem Text Text effs
test1WebTxt =
  Test
    { config =
        TestConfig
          { title = "test1WebTxt",
            channel = Web
          },
      items = txtItems 5,
      interactor = \_ _ _ -> L.log "Hello" >> pure "Hi",
      parse = emptiParser "Blahh"
    }

test2RESTInt :: MockTest Int IntItem Int Int effs
test2RESTInt =
  Test
    { config =
        TestConfig
          { title = "test2RESTInt",
            channel = REST
          },
      items = empti,
      interactor = emptiInteractor 44,
      parse = pure
    }

test3RESTInt :: MockTest Int IntItem Int Int effs
test3RESTInt =
  Test
    { config =
        TestConfig
          { title = "test3RESTInt",
            channel = REST
          },
      items = empti,
      interactor = emptiInteractor 3,
      parse = pure
    }

test4WebTxt :: MockTest Text TextItem Text Text effs
test4WebTxt =
  Test
    { config =
        TestConfig
          { title = "test4WebTxt",
            channel = Web
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = pure
    }

test6WebTxt :: MockTest Text TextItem Text Text effs
test6WebTxt =
  Test
    { config =
        TestConfig
          { title = "test6WebTxt",
            channel = Web
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = pure
    }

test5RESTTxt :: MockTest Text TextItem Text Text effs
test5RESTTxt =
  Test
    { config =
        TestConfig
          { title = "test5RESTTxt",
            channel = REST
          },
      items = empti,
      interactor = emptiInteractor "Hello for REST 5",
      parse = pure
    }

tossFilter :: TestFilter RunConfig TestConfig
tossFilter =
  TestFilter
    { title = \RunConfig {target} _ TestConfig {channel} -> "toss call: " <> txt channel <> " must match run: " <> txt target,
      predicate = \RunConfig {target} _ TestConfig {channel} -> case target of
        AllChannels -> True
        WebOnly -> channel == Web
        RESTOnly -> channel == REST
    }

hasTitle :: Maybe Text -> TestFilter RunConfig TestConfig
hasTitle mbTtl =
  TestFilter
    { title = \_ _ _ -> maybef mbTtl "test title N/A - no test fragmant provided" ("test title must include: " <>),
      predicate = \_ _ TestConfig {title = testTtl} ->
        maybef
          mbTtl
          True
          \ttl' -> toLower ttl' `isInfixOf` toLower testTtl
    }

mockSuite :: forall effs a. DemoEffs effs => SuiteSource Text TestConfig RunConfig effs a
mockSuite runTest =
  R.TestSuite
    [ R.Group
        "Filter SuiteSource"
        [ BeforeAll
            "Before All"
            ( \_ -> do
                L.log "hello"
                pure "hello"
            )
            [ R.Group
                "Divider"
                [ Tests
                    \a hd ->
                      [ runTest a hd test1WebTxt,
                        runTest a hd test4WebTxt
                      ]
                ],
              ----
              R.Group
                "Empty Group"
                [Tests \_ _ -> []],
              ----
              R.Group
                "Divider"
                [ BeforeAll
                    "Before Inner"
                    (\_ -> pure "HI")
                    [ Tests \a hd ->
                        [ runTest a hd test6WebTxt,
                          runTest a hd test5RESTTxt
                        ]
                    ]
                ]
            ]
        ],
      R.Group
        { title = "Nested Int Group",
          gElms =
            [ BeforeAll
                { title = "Int BE",
                  bHook = (\i -> pure 23) :: () -> Sem effs Int,
                  bhElms =
                    [ Tests \a hd ->
                        [ 
                          runTest a hd test2RESTInt,
                          runTest a hd test3RESTInt
                        ],
                      AfterAll
                        { title = "Clean up",
                          aHook = \_ -> L.log "Clean Up",
                          ahElms =
                            [ BeforeAll
                                { title = "Before Inner 2",
                                  bHook = \t -> pure "HI",
                                  bhElms =
                                    [ Tests \a hd ->
                                        [ runTest a hd test6WebTxt
                                        ],
                                      R.Group
                                        { title = "Double Nested Empty Group",
                                          gElms = []
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        }
    ]

filters' :: Maybe Text -> [TestFilter RunConfig TestConfig]
filters' ttl = [tossFilter, hasTitle ttl]

mockRun :: forall effs. DemoEffs effs => Text -> ChannelSelect -> Maybe Text -> Sem effs ()
mockRun runTitle targChannel testTitleFilterFragment =
  mkSem $
    RunParams
      { suite = mockSuite,
        filters = filters' testTitleFilterFragment,
        itemIds = Nothing,
        itemRunner = runItem,
        rc =
          RunConfig
            { title = runTitle,
              target = targChannel
            }
      }

everythingRun :: forall effs. DemoEffs effs => Sem effs ()
everythingRun = mockRun "Run All" AllChannels Nothing

webRun :: forall effs. DemoEffs effs => Sem effs ()
webRun = mockRun "Web Tests" WebOnly Nothing

restRun :: forall effs. DemoEffs effs => Sem effs ()
restRun = mockRun "Web Tests" RESTOnly Nothing

txtRun :: forall effs. DemoEffs effs => Sem effs ()
txtRun = mockRun "Run REST" AllChannels $ Just "txt"


$(deriveJSON defaultOptions ''Channel)
$(deriveJSON defaultOptions ''ChannelSelect)
$(deriveJSON defaultOptions ''RunConfig)

-- unit_test_filter_expect_empty