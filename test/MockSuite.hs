-- {-# LANGUAGE NoStrictData #-}

module MockSuite where

import DSL.Interpreter (MinEffs)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import ItemRunners (runItem)
import Polysemy
import Pyrelude as P
import Pyrelude.Test hiding (Group)
import Runner as R
import RunnerBase (IsRoot, Test)
import TestFilter

data Include = In | Out deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''Include)

data RunConfig = RunConfig
  { title :: Text,
    inFilter :: Bool
  }
  deriving (Eq, Show)

inFilterRunConfig :: RunConfig
inFilterRunConfig = RunConfig "In Filter" True

outOfFilterRunConfig :: RunConfig
outOfFilterRunConfig = RunConfig "In Filter" False

inOutFilter :: TestFilter RunConfig TestConfig
inOutFilter =
  TestFilter
    { title = "in out filter state must match",
      predicate = \rc _ tc -> inFilter rc == (include tc == In)
    }

instance HasTitle RunConfig

instance Config RunConfig

type DemoEffs effs = MinEffs Text effs

data TestConfig = TestConfig
  { title :: Text,
    include :: Include
  }
  deriving (Show, Eq)

instance Config TestConfig

instance HasTitle TestConfig

$(deriveJSON defaultOptions ''TestConfig)

--    e      tc        rc       hi i as ds effs
type MockTest hi i as ds effs = RunnerBase.Test Text TestConfig RunConfig hi i as ds effs

data IntItem = IntItem {
  id :: Int,
  title :: Text,
  checks :: C.Checks Text
} deriving (Show, Generic)

data TextItem = TextItem {
  id :: Int,
  title :: Text,
  checks :: C.Checks Int
} deriving (Show, Generic)

instance ToJSON TextItem where
  toEncoding = genericToEncoding defaultOptions

instance ItemClass IntItem 

instance ItemClass TextItem TextItem

instance ToJSON IntItem where
  toEncoding = genericToEncoding defaultOptions

empti :: a -> [b]
empti = const ([] :: [b])

--                 as ->    rc     -> hi -> i -> Sem effs as
emptiInteractor :: as -> RunConfig -> hi -> i -> Sem effs as
emptiInteractor as _ _ _ = pure as

emptiParser :: a -> as -> Sem effs a
emptiParser a _ = pure a

test1Txt :: MockTest Text IntItem Text IntItem effs
test1Txt =
  Test
    { config =
        TestConfig
          { title = "test1",
            include = In
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = emptiParser $ IntItem 1 1
    }

test2Int :: MockTest Int IntItem IntItemIntItemeffs
test2Int =
  Test
    { config =
        TestConfig
          { title = "test2",
            include = In
          },
      items = empti,
      interactor = emptiInteractor $ IntItem 2 1,
      parse = pure
    }

test3Bool :: MockTest Bool IntItem IntItemIntItemeffs
test3Bool =
  Test
    { config =
        TestConfig
          { title = "test3",
            include = Out
          },
      items = empti,
      interactor = emptiInteractor $ IntItem 3 2,
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
          { title = "test4",
            include = In
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = pure
    }

test5Int :: MockTest Int IntItem IntItemIntItemeffs
test5Int =
  Test
    { config =
        TestConfig
          { title = "test5",
            include = Out
          },
      items = empti,
      interactor = emptiInteractor (IntItem4 1),
      parse = pure
    }

includeFilter :: TestFilter RunConfig TestConfig
includeFilter =
  TestFilter
    { title = "test include must match run",
      predicate = \(RunConfig _ inc) _ tc -> (include tc == In) == inc
    }

filters' :: [TestFilter RunConfig TestConfig]
filters' = [includeFilter]

mockSuite :: forall effs a. (forall hi i as ds. (Show i, Show as, Show ds) => Address -> hi -> MockTest hi i as ds effs -> a) -> SuiteItem IsRoot () effs [a]
mockSuite r =
  R.Root
    [ R.Group
        "Filter TestSuite"
        [ BeforeHook
            { title = "Before All",
              cardinality = ExeOnce,
              bHook = pure "hello",
              bhElms =
                [ \a o ->
                    Tests
                      [ r a o test1Txt,
                        r a o test4Txt
                      ],
                  \a o ->
                    R.Group
                      { title = "Empty Group",
                        gElms =
                          [ Tests []
                          ]
                      },
                  \a o ->
                    BeforeHook
                      { title = "Before Inner",
                        cardinality = ExeOnce,
                        bHook = pure o,
                        bhElms =
                          [ \a' o' ->
                              Tests
                                [ r a' o' test6Txt
                                ]
                          ]
                      }
                ]
            },
          R.Group
            { title = "Nested Int Group",
              gElms =
                [ BeforeHook
                    { title = "Int Group",
                      cardinality = ExeForEach,
                      bHook = pure 23,
                      bhElms =
                        [ \a t ->
                            AfterHook
                              { title = "After Exch Int",
                                cardinality = ExeForEach,
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

runParams :: forall effs. DemoEffs effs => RunParams Maybe Text RunConfig TestConfig effs ()
runParams =
  RunParams
    { suite = mockSuite,
      filters = filters',
      itemIds = Nothing,
      itemRunner = runItem,
      rc = RunConfig "Happy TestSuite" True
    }

happyRun :: forall effs. DemoEffs effs => Sem effs ()
happyRun = mkSem runParams

$(deriveJSON defaultOptions ''RunConfig)

-- unit_test_filter_expect_empty