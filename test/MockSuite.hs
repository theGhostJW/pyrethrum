module MockSuite where

import AuxFiles ()
import Common ()
import DSL.Interpreter (MinEffs, executeForTest, minInterpret)
import DSL.LogProtocol (LogProtocolBase)
import DSL.LogProtocol.PrettyPrint ()
import Data.Aeson
import Data.Aeson.TH
import ItemRunners
import LogTransformation.Common ()
import Polysemy
import Pyrelude
import qualified Pyrelude as P
import Pyrelude.IO ()
import Pyrelude.Test as T ()
import RunElementClasses
import Runner as R
import qualified RunnerBase
import TestFilter

type AppError = FrameworkError Text
type LogProtocol = LogProtocolBase AppError

newtype RunConfig = RunConfig
  { include :: Bool
  }
  deriving (Eq, ToJSON, Show, FromJSON)

instance RunConfigClass RunConfig

instance Titled RunConfig where
  title rc = toS $ show rc

data TestConfig = TestConfig
  { header :: Text,
    address :: TestAddress,
    include :: Bool
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestConfig)

instance TestConfigClass TestConfig where
  moduleAddress = address

instance Titled TestConfig where
  title = header

type MockTest i as ds effs = RunnerBase.Test MyText TestConfig RunConfig i as ds effs

newtype MyInt = MyInt Int deriving (Show, Generic)

instance ToJSON MyInt where
  toEncoding = genericToEncoding defaultOptions

newtype MyText = MyText Text deriving (Show, Generic, ToJSON)

instance ItemClass MyInt MyText where
  identifier _ = -999
  whenClause _ = "pre"
  thenClause _ = "post"
  checkList = mempty

instance ItemClass MyText MyText where
  identifier _ = -999
  whenClause _ = "pre"
  thenClause _ = "post"
  checkList = mempty

empti :: a -> [b]
empti = const ([] :: [b])

emptiInteractor :: b -> RunConfig -> a -> Sem effs b
emptiInteractor b _ _ = pure b

emptiParser :: a -> i -> as -> Sem effs a
emptiParser a _ _ = pure a

test1 :: MockTest MyInt Text MyText effs
test1 =
  RunnerBase.Test
    { config =
        TestConfig
          { header = "test1",
            address = TestAddress "test1",
            include = True
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = pure . MyText . txt
    }

test2 :: MockTest MyInt Int MyText effs
test2 =
  Test
    { config =
        TestConfig
          { header = "test2",
            address = TestAddress "test2 address",
            include = True
          },
      items = empti,
      interactor = emptiInteractor 1,
      parse = pure . MyText . txt
    }

test3 :: MockTest MyInt Int MyText effs
test3 =
  Test
    { config =
        TestConfig
          { header = "test3",
            address = TestAddress "test3 address",
            include = True
          },
      items = empti,
      interactor = emptiInteractor 3,
      parse = pure . MyText . txt
    }

test4 :: MockTest MyText Text MyText effs
test4 =
  Test
    { config =
        TestConfig
          { header = "test4",
            address = TestAddress "test4 address",
            include = True
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = pure . MyText . txt
    }

test5 :: MockTest MyInt Int MyText effs
test5 =
  Test
    { config =
        TestConfig
          { header = "test5",
            address = TestAddress "test5 address",
            include = True
          },
      items = empti,
      interactor = emptiInteractor 1,
      parse = pure . MyText . txt
    }

includeFilter :: TestFilter RunConfig TestConfig
includeFilter =
  TestFilter
    { title = "test must be is enabled",
      predicate = \rc tc -> (include :: TestConfig -> Bool) tc == (include :: RunConfig -> Bool) rc
    }

filters' :: [TestFilter RunConfig TestConfig]
filters' = [includeFilter]

demoSuit ::  SuiteItem effs [Text]
demoSuit =
  R.Group
    "Happy Suite"
    [ Hook
        BeforeAll
        (pure ())
        [ Tests
            [ "test 1",
              "test 2",
              "test 3"
            ]
        ],
      R.Group
        "Sub Group"
        [ Tests
            [ 
              "test 4",
              "test 5"
            ]
        ],
      R.Group
        "Empty Group"
        [ 
          Tests []
        ]
    ]

happySuite :: forall effs a. (forall i as ds. (Show i, Show as, Show ds, ToJSON as, ToJSON ds, ItemClass i ds) => MockTest i as ds effs -> a) -> SuiteItem effs [a]
happySuite r =
  R.Group
    "Filter Suite"
    [ Hook
        BeforeAll
        (pure ())
        [ Tests
            [ r test1,
              r test2,
              r test3
            ]
        ],
      R.Group
        "Sub Group"
        [ Tests
            [ r test4,
              r test5
            ]
        ],
      R.Group
        "Empty Group"
        [ 
          Tests []
        ]
    ]

runParams :: forall effs. MinEffs MyText effs => RunParams Maybe MyText RunConfig TestConfig effs
runParams =
  RunParams
    { suite = happySuite,
      filters = filters',
      itemIds = Nothing,
      itemRunner = runItem,
      rc = RunConfig True
    }

happyRun :: forall effs. MinEffs MyText effs => Sem effs ()
happyRun = mkRunSem runParams