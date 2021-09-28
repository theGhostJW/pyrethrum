-- {-# LANGUAGE NoStrictData #-}

module MockSuite where

import qualified Check as C
import DSL.Interpreter (MinEffs)
import Data.Aeson.TH
import Data.Aeson.Types hiding (One)
import Data.Yaml
import ItemRunners (runItem)
import Polysemy
import Pyrelude as P
-- import Pyrelude.Test hiding (Group, maybe)
-- import Pyrelude.Test hiding (Group, maybe)
-- import Pyrelude.Test hiding (Group, maybe)
-- import Pyrelude.Test hiding (Group, maybe)
import Runner as R
  ( Address,
    Config,
    One,
    RunParams (..),
    SuiteItem (..),
    Test (..),
    mkSem, ItemClass
  )
import TestFilter
import GHC.Records (HasField)
import qualified Check

data TossCall = Heads | Tails deriving (Eq, Ord, Show)

data TossResult = RcHeads | RcTails | RcAll deriving (Eq, Ord, Show)

data RunConfig = RunConfig
  { title :: Text,
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
type MockTest hi i as ds effs = Test Text TestConfig RunConfig hi i as ds effs

data IntItem = IntItem
  { id :: Int,
    title :: Text,
    checks :: C.Checks Int
  }
  deriving (Show, Generic)

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

test1Txt :: MockTest Text TextItem Text Text effs
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

test4Txt :: MockTest Text TextItem Text Text effs
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

test6Txt :: MockTest Text TextItem Text Text effs
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
    { title = \RunConfig {toss} _ TestConfig {tossCall} -> "toss call: " <> txt tossCall <> " must match run: " <> txt toss,
      predicate = \RunConfig {toss} _ TestConfig {tossCall} -> case toss of
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


mockSuite :: forall effs a. (forall hi i as ds. (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, HasField "id" i Int, HasField "title" i Text, HasField "checks" i (Check.Checks ds)) => Address -> hi -> MockTest hi i as ds effs -> a) -> SuiteItem One () () effs [a]
mockSuite runTest =
  R.Root
    [ 
      R.Group
        "Filter TestSuite"
        [ \a i ->
            BeforeAll
              "Before All"
              (\i' -> pure "hello")
              [ \a1 o ->
                  R.Group
                    "Divider"
                    [ \a1' o' ->
                        Tests
                          [ runTest a1' o' test1Txt,
                            runTest a1' o' test4Txt
                          ]
                    ],
                \a1 o ->
                  R.Group
                    "Empty Group"
                    [\_ _ -> Tests []],
                \a1 o ->
                  R.Group
                    "Divider"
                    [ \a1' o' ->
                        BeforeEach
                          "Before Inner"
                          (\t -> pure o)
                          [ \a'' o'' ->
                              Tests
                                [ runTest a'' o'' test6Txt
                                ]
                          ]
                    ]
              ]
        ],
      R.Group
        { title = "Nested Int Group",
          gElms =
            [ \a1 s ->
                BeforeEach
                  { title' = "Int Group",
                    bHook' = \i' -> pure 23,
                    bhElms' =
                      [ \a2 t ->
                          AfterEach
                            { title' = "After Exch Int",
                              aHook' = \_ -> t == 23 ? pure () $ pure (),
                              ahElms' =
                                [ \a3 i' ->
                                    Tests
                                      [
                                        runTest a3 i' test5Int,
                                        runTest a3 i' test2Int,
                                        runTest a3 i' test3Int
                                      ]
                                ]
                            }
                      ]
                  }
            ]
        }
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