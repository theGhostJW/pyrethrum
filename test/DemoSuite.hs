-- {-# LANGUAGE NoStrictData #-}

module DemoSuite where

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
import Data.Yaml
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
import TestFilter

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
test1WebTxt :: forall effs. Member (Logger Text) effs => DemoTest Text TextItem Text Text effs
test1WebTxt =
  Test
    { config =
        TestConfig
          { title = "test1WebTxt",
            channel = Web
          },
      items = txtItems 5,
      interactor = interact "test1WebTxt",
      parse = emptiParser "Blahh"
    }

test2RESTInt :: forall effs. Member (Logger Text) effs => DemoTest Int IntItem Int Int effs
test2RESTInt =
  Test
    { config =
        TestConfig
          { title = "test2RESTInt",
            channel = REST
          },
      items = intItems 2,
      interactor = interact "test2RESTInt",
      parse = pure
    }

test3RESTInt :: forall effs. Member (Logger Text) effs => DemoTest Int IntItem Int Int effs
test3RESTInt =
  Test
    { config =
        TestConfig
          { title = "test3RESTInt",
            channel = REST
          },
      items = empti,
      interactor = interact "test3RESTInt",
      parse = pure
    }

test4WebTxt :: forall effs. Member (Logger Text) effs => DemoTest Text TextItem Text Text effs
test4WebTxt =
  Test
    { config =
        TestConfig
          { title = "test4WebTxt",
            channel = Web
          },
      items = empti,
      interactor = interact "test4WebTxt",
      parse = pure
    }

test6WebTxt :: forall effs. Member (Logger Text) effs => DemoTest Text TextItem Text Text effs
test6WebTxt =
  Test
    { config =
        TestConfig
          { title = "test6WebTxt",
            channel = Web
          },
      items = empti,
      interactor = interact "test6WebTxt",
      parse = pure
    }

test61WebTxt :: forall effs. Member (Logger Text) effs => DemoTest Text TextItem Text Text effs
test61WebTxt =
  Test
    { config =
        TestConfig
          { title = "test61WebTxt",
            channel = Web
          },
      items = txtItems 1,
      interactor = interact "test61WebTxt",
      parse = pure
    }



test5RESTTxt :: forall effs. Member (Logger Text) effs => DemoTest Text TextItem Text Text effs
test5RESTTxt =
  Test
    { config =
        TestConfig
          { title = "test5RESTTxt",
            channel = REST
          },
      items = txtItems 2,
      interactor = interact "test5RESTTxt",
      parse = pure
    }

channelFilter :: TestFilter RunConfig TestConfig
channelFilter =
  TestFilter
    { title = \RunConfig {target} _ TestConfig {channel} -> "channel: " <> txt channel <> " must match run: " <> txt target,
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

demoSuite :: forall effs a. DemoEffs effs => SuiteSource Text TestConfig RunConfig effs a
demoSuite runTest =
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
                        },
                      ----
                      R.Group
                        { title = "Group 1 >> Empty Group 2",
                          gElms = [Tests \_ _ -> []]
                        },
                      ----
                      R.Group
                        { title = "Group 1 >> Group 3",
                          gElms =
                            [ OnceHook
                                { title = "Group 1 >> Group 3 >> Before 1.1",
                                  bHook = \_ -> L.log "BH - Group 1 >> Group 3 >> Before Hook 1.1" $> "Hello",
                                  hkElms =
                                    [ Tests \a hd ->
                                        [ runTest a hd test61WebTxt,
                                          runTest a hd test5RESTTxt
                                        ]
                                    ],
                                  aHook = \_ -> L.log "AH - Group 1 >> Group 3 >> After Hook 1.1"
                                }
                            ]
                        }
                    ],
                  aHook = \_ -> L.log "AH - Group 1 >> After Hook 1"
                },
              R.Group
                { title = "Group 1 >> Group 2",
                  gElms =
                    [ OnceHook
                        { title = "Group 1 >> Group 2 >> Hook 1",
                          bHook = \i -> L.log "BH - Group 1 >> Group 2 >> Before Hook 1" $> 23,
                          hkElms =
                            [ Tests \a hd ->
                                [ runTest a hd test2RESTInt,
                                  -- no test items
                                  runTest a hd test3RESTInt
                                ]
                            ],
                          aHook = \i -> L.log "AH - Group 1 >> Group 2 >> After Hook 1"
                        },
                      OnceHook 
                        -- this is an empty hook should not run
                        { title = "Group 1 >> Group 2 >> Hook 2",
                          bHook = \_ -> L.log "BH - Group 1 >> Group 2 >> Before Hook 2" $> "Hello",
                          hkElms =
                            [ OnceHook
                                { title = "Group 1 >> Group 2 >> Hook 2 >> Hook 3",
                                  bHook = \i -> L.log "BH - Group 1 >> Group 2 >> Hook 2 >> Hook 3 Before" $> "Hi",
                                  hkElms =
                                    [ Tests \a hd ->
                                        [ -- no test items
                                          runTest a hd test6WebTxt
                                        ],
                                      R.Group
                                        { title = "Double Nested Empty Group",
                                          gElms = []
                                        }
                                    ],
                                  aHook = \_ -> L.log "AH - Group 1 >> Group 2 >> Hook 2 >> After Hook 3" 
                                }
                            ],
                          aHook = \s ->  L.log "AH - Group 1 >> Group 2 >> After Hook 2" 
                        }
                    ]
                }
            ]
        }
    ]



filters' :: Maybe Text -> [TestFilter RunConfig TestConfig]
filters' ttl = [channelFilter, hasTitle ttl]

demoRun :: forall effs. DemoEffs effs => Text -> ChannelSelect -> Maybe Text -> Sem effs ()
demoRun runTitle targChannel testTitleFilterFragment =
  mkSem $
    RunParams
      { suite = demoSuite,
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
everythingRun = demoRun "Run All" AllChannels Nothing

webRun :: forall effs. DemoEffs effs => Sem effs ()
webRun = demoRun "Web Tests" WebOnly Nothing

restRun :: forall effs. DemoEffs effs => Sem effs ()
restRun = demoRun "Web Tests" RESTOnly Nothing

txtRun :: forall effs. DemoEffs effs => Sem effs ()
txtRun = demoRun "Run REST" AllChannels $ Just "txt"

$(deriveJSON defaultOptions ''Channel)
$(deriveJSON defaultOptions ''ChannelSelect)
$(deriveJSON defaultOptions ''RunConfig)

-- unit_test_filter_expect_empty