-- {-# LANGUAGE NoStrictData #-}

module MockSuite where

-- import Pyrelude.Test hiding (Group, maybe)
-- import Pyrelude.Test hiding (Group, maybe)
-- import Pyrelude.Test hiding (Group, maybe)
-- import Pyrelude.Test hiding (Group, maybe)

import qualified Check
import qualified Check as C
import DSL.Interpreter (AllEffects, MinEffs, Failure)
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
    Suite (..),
    SuiteItem (..),
    Test (..),
    mkSem, TestSuite
  )
import TestFilter
import DSL.Logger as L
import DSL.FileSystem
import DSL.ArbitraryIO
import DSL.CurrentTime

data TossCall = Heads | Tails deriving (Eq, Ord, Show)

data TossResult = RcHeads | RcTails | RcAll deriving (Eq, Ord, Show)

rcRunAll = RunConfig "Run Everything" RcAll

data RunConfig = RunConfig
  { title :: Text,
    toss :: TossResult
  }
  deriving (Eq, Show)

instance Config RunConfig

-- type DemoEffs effs = MinEffs Text effs
type DemoEffs effs = AllEffects Text effs

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

---                           hi  itm     as   ds
test1HeadsTxt :: forall effs. Member (Logger Text) effs =>  MockTest Text TextItem Text Text effs
test1HeadsTxt =
  Test
    { config =
        TestConfig
          { title = "test1HeadsTxt",
            tossCall = Heads
          },
      items = empti,
      interactor = \_ _ _  ->  L.log "Hello" >> pure "Hi",
      parse = emptiParser "Blahh"
    }

test2TailsInt :: MockTest Int IntItem Int Int effs
test2TailsInt =
  Test
    { config =
        TestConfig
          { title = "test2TailsInt",
            tossCall = Tails
          },
      items = empti,
      interactor = emptiInteractor 44,
      parse = pure
    }

test3TailsInt :: MockTest Int IntItem Int Int effs
test3TailsInt =
  Test
    { config =
        TestConfig
          { title = "test3TailsInt",
            tossCall = Tails
          },
      items = empti,
      interactor = emptiInteractor 3,
      parse = pure
    }

test4HeadsTxt :: MockTest Text TextItem Text Text effs
test4HeadsTxt =
  Test
    { config =
        TestConfig
          { title = "test4HeadsTxt",
            tossCall = Heads
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = pure
    }

test6HeadsTxt :: MockTest Text TextItem Text Text effs
test6HeadsTxt =
  Test
    { config =
        TestConfig
          { title = "test6HeadsTxt",
            tossCall = Heads
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = pure
    }

test5TailsInt :: MockTest Int IntItem Int Int effs
test5TailsInt =
  Test
    { config =
        TestConfig
          { title = "test5TailsInt",
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

mockSuite ::
  forall effs a. Members '[FileSystem, ArbitraryIO, Logger Text, CurrentTime, Failure Text] effs => 
  ( forall hd i as ds.
    (Show i, ToJSON i, Show as, ToJSON as, Show ds, ToJSON ds, ItemClass i ds) =>
    Address ->
    hd ->
    MockTest hd i as ds effs ->
    a
  ) ->
  Suite () effs a
mockSuite runTest =
  R.Suite
    [ R.Group
        "Filter TestSuite"
        [ BeforeAll 
            "Before All"
            (\_ -> do 
               L.log "hello"
               pure "hello" 
            )
            [ R.Group
                "Divider"
                [ Tests
                    \a hd ->
                      [ runTest a hd test1HeadsTxt,
                        runTest a hd test4HeadsTxt
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
                        [ runTest a hd test6HeadsTxt
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
                        [ runTest a hd test5TailsInt,
                          runTest a hd test2TailsInt,
                          runTest a hd test3TailsInt
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
                                        [ runTest a hd test6HeadsTxt
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