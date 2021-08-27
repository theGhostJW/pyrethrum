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
import RunnerBase (IsRoot, Test)
import TestFilter

data Include = In | Out deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''Include)

data RunConfig = RunConfig
  { title :: Text,
    includeFlag :: Maybe Bool,
    testTitle :: Maybe Text
  }
  deriving (Eq, Show)

includeFlagRunConfig :: RunConfig
includeFlagRunConfig =
  RunConfig
    { title = "Include Flag",
      includeFlag = Just True,
      testTitle = Nothing
    }

excludeFlagRunConfig :: RunConfig
excludeFlagRunConfig =
  RunConfig
    { title = "Exclude Flag",
      includeFlag = Just False,
       testTitle = Nothing
    }


instance Config RunConfig

type DemoEffs effs = MinEffs Text effs

data TestConfig = TestConfig
  { title :: Text,
    include :: Include
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
            include = In
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
            include = In
          },
      items = empti,
      interactor = emptiInteractor 44,
      parse = pure
    }

test3Bool :: MockTest Int IntItem Int Int effs
test3Bool =
  Test
    { config =
        TestConfig
          { title = "test3",
            include = Out
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
            include = In
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
            include = In
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
            include = Out
          },
      items = empti,
      interactor = emptiInteractor 22,
      parse = pure
    }

includeFilter :: TestFilter RunConfig TestConfig
includeFilter =
  TestFilter
    { title = "test include must match run",
      predicate = \RunConfig {includeFlag} _ tc -> maybe True (\incRc -> (include tc == In) == incRc) includeFlag
    }

hasTitle :: Maybe Text -> TestFilter RunConfig TestConfig
hasTitle ttl =
  TestFilter
    { title = maybef ttl "test title includes N/A - no test fragmant provided" ("test title includes " <>),
      predicate = \RunConfig {testTitle} _ tc -> maybe True (\frag -> frag `isInfixOf` toLower ((title :: TestConfig -> Text ) tc)) testTitle
    }

mockSuite :: forall effs a. (forall hi i as ds. (Show i, Show as, Show ds) => Address -> hi -> MockTest hi i as ds effs -> a) -> SuiteItem IsRoot () effs [a]
mockSuite r =
  R.Root
    [ R.Group "Filter TestSuite"

        [ BeforeAll "Before All"
            (pure "hello")
            [ \a o ->
                Tests
                  [ r a o test1Txt,
                    r a o test4Txt
                  ],

              \a o ->
                R.Group "Empty Group"
                  [Tests []],

              \a o ->
                BeforeEach "Before Inner"
                  ( pure o)
                  [
                    \a' o' ->
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
                                          r a2 i test2Int
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
filters' ttl = [includeFilter, hasTitle ttl]

runParams :: forall a effs. DemoEffs effs => Maybe Text -> RunParams Maybe Text RunConfig TestConfig effs a
runParams nameTxt =
  RunParams
    { suite = mockSuite,
      filters = filters' nameTxt,
      itemIds = Nothing,
      itemRunner = runItem,
      rc = RunConfig {
        title = "Happy Test Run",
        includeFlag = Just True,
        testTitle = Nothing
        }
    }

mockRun :: forall effs. DemoEffs effs => Maybe Text -> Sem effs ()
mockRun nameTxt = mkSem $ runParams nameTxt

happyRun :: forall effs. DemoEffs effs => Sem effs ()
happyRun = mockRun Nothing

$(deriveJSON defaultOptions ''RunConfig)

-- unit_test_filter_expect_empty