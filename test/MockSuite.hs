-- {-# LANGUAGE NoStrictData #-}

module MockSuite where

import qualified Check as C
import DSL.Interpreter (MinEffs)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import ItemRunners (runItem)
import Polysemy
import Pyrelude as P
import Pyrelude.Test hiding (Group, maybe)
import Runner as R
    ( Config,
      Address,
      SuiteItem(Tests, Root, BeforeAll, Group, BeforeEach, AfterEach,
                gElms, bHook, bhElms, title, aHook, ahElms),
      Test(Test, config, items, interactor, parse),
      mkSem,
      RunParams(..) )
import RunnerBase (IsRoot, Test)
import TestFilter

data TossCall =  Heads | Tails deriving (Eq, Ord, Show)

data TossResult = RcHeads | RcTails | RcAll deriving (Eq, Ord, Show)


data RunConfig = RunConfig
  {
    title :: Text,
    toss :: TossResult
  }
  deriving (Eq, Show)

instance Config RunConfig

type DemoEffs effs = MinEffs Text effs

data TestConfig = TestConfig
  { title :: Text,
    tossCall :: TossCall
  }
  deriving (Show, Eq)

instance Config TestConfig

$(deriveJSON defaultOptions ''TestConfig)

-- | A standard test
type MockTest hi i as ds effs = RunnerBase.Test Text TestConfig RunConfig hi i as ds effs

data IntItem = IntItem
  { id :: Int,
    title :: Text,
    checks :: C.Checks Text
  }
  deriving (Show, Generic)

data TextItem = TextItem
  { id :: Int,
    title :: Text,
    checks :: C.Checks Int
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

emptiParser :: ds -> as -> Sem effs ds
emptiParser ds _ = pure ds

test1Txt :: MockTest Text IntItem Text Text effs
test1Txt =
  Test
    { config =
        TestConfig
          { title = "test1",
            tossCall = Heads
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = emptiParser "Blahh"
    }

test2Int :: MockTest Int IntItem Int Int effs
test2Int =
  Test
    { config =
        TestConfig
          { title = "test2",
            tossCall = Tails
          },
      items = empti,
      interactor = emptiInteractor 44,
      parse = pure
    }

test3Int :: MockTest Int IntItem Int Int effs
test3Int =
  Test
    { config =
        TestConfig
          { title = "test3",
            tossCall = Tails
          },
      items = empti,
      interactor = emptiInteractor 3,
      parse = pure
    }

test4Txt :: MockTest Text Text Text Text effs
test4Txt =
  Test
    { config =
        TestConfig
          { title = "test4",
            tossCall = Heads
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = pure
    }

test6Txt :: MockTest Text Text Text Text effs
test6Txt =
  Test
    { config =
        TestConfig
          { title = "test6",
            tossCall = Heads
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = pure
    }

test5Int :: MockTest Int IntItem Int Int effs
test5Int =
  Test
    { config =
        TestConfig
          { title = "test5",
            tossCall = Tails
          },
      items = empti,
      interactor = emptiInteractor 22,
      parse = pure
    }

tossFilter :: TestFilter RunConfig TestConfig
tossFilter =
  TestFilter
    { title = \RunConfig { toss } _ TestConfig { tossCall } -> "toss call: " <>  txt tossCall <> " must match run: " <> txt toss,
      predicate = \RunConfig {toss} _ TestConfig{ tossCall } -> case toss of
                                                      RcAll -> True
                                                      RcHeads -> tossCall == Heads
                                                      RcTails -> tossCall == Tails
    }

hasTitle :: Maybe Text -> TestFilter RunConfig TestConfig
hasTitle ttl =
  TestFilter
    { title = \_ _ _ -> maybef ttl "test title N/A - no test fragmant provided" ("test title must include: " <>),
      predicate = \_ _ TestConfig {title = testTtl} ->
        maybef
          ttl
          True
          \ttl' -> toLower ttl' `isInfixOf` toLower testTtl
    }

mockSuite :: forall effs a. (forall hi i as ds. (Show i, Show as, Show ds) => Address -> hi -> MockTest hi i as ds effs -> a) -> SuiteItem IsRoot () effs [a]
mockSuite r =
  R.Root
    [ R.Group
        "Filter TestSuite"
        [ BeforeAll
            "Before All"
            (pure "hello")
            [ \a o ->
                Tests
                  [ r a o test1Txt,
                    r a o test4Txt
                  ],
              \a o ->
                R.Group
                  "Empty Group"
                  [Tests []],
              \a o ->
                BeforeEach
                  "Before Inner"
                  (pure o)
                  [ \a' o' ->
                      Tests
                        [ r a' o' test6Txt
                        ]
                  ]
            ],
          R.Group
            { title = "Nested Int Group",
              gElms =
                [ BeforeEach
                    { title = "Int Group",
                      bHook = pure 23,
                      bhElms =
                        [ \a t ->
                            AfterEach
                              { title = "After Exch Int",
                                aHook = t == 23 ? pure () $ pure (),
                                ahElms =
                                  [ \a2 i ->
                                      Tests
                                        [ r a2 i test5Int,
                                          r a2 i test2Int,
                                          r a2 i test3Int
                                        ]
                                  ]
                              }
                        ]
                    }
                ]
            }
        ]
    ]

filters' :: Maybe Text -> [TestFilter RunConfig TestConfig]
filters' ttl = [tossFilter, hasTitle ttl]

runParams :: forall effs. DemoEffs effs => Maybe Text -> RunParams Maybe Text RunConfig TestConfig effs
runParams nameTxt =
  RunParams
    { suite = mockSuite,
      filters = filters' nameTxt,
      itemIds = Nothing,
      itemRunner = runItem,
      rc =
        RunConfig
          { title = "Happy Test Run",
            toss = RcHeads
          }
    }

mockRun :: forall effs. DemoEffs effs => Maybe Text -> Sem effs ()
mockRun nameTxt = mkSem $ runParams nameTxt

happyRun :: forall effs. DemoEffs effs => Sem effs ()
happyRun = mockRun Nothing

$(deriveJSON defaultOptions ''TossCall)
$(deriveJSON defaultOptions ''TossResult)
$(deriveJSON defaultOptions ''RunConfig)

-- unit_test_filter_expect_empty