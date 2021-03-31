module MockSuite where

import AuxFiles ()
import Common ()
import DSL.Interpreter (MinEffs, executeForTest, minInterpret)
import DSL.LogProtocol (LogProtocolBase)
import DSL.LogProtocol.PrettyPrint ()
import Data.Aeson
    ( FromJSON, ToJSON(toEncoding), defaultOptions, genericToEncoding )
import Data.Aeson.TH
import ItemRunners
import LogTransformation.Common ()
import Polysemy
import Pyrelude
import qualified Pyrelude as P
import Pyrelude.IO ()
import Pyrelude.Test as T ()
import RunElementClasses
    ( TestAddress(TestAddress),
      ItemClass(checkList, thenClause, whenClause, identifier),
      TestConfigClass(..),
      RunConfigClass,
      Titled(..) )
import Runner as R
    ( FrameworkError,
      mkRunSem,
      RunParams(..),
      HookLocation(BeforeAll),
      SuiteItem(Tests, Hook, Group),
      Test(Test, config, items, interactor, parse))
      
import qualified RunnerBase
import TestFilter ( TestFilter(..) )
import DSL.Logger

type AppError = FrameworkError Text
type LogProtocol = LogProtocolBase AppError

data RunConfig = RunConfig
  { 
    cfgHeader :: Text,
    include :: Bool
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''RunConfig)

instance RunConfigClass RunConfig

instance Titled RunConfig where
  title rc = cfgHeader rc

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

type MockTest i as ds effs = RunnerBase.Test Text TestConfig RunConfig i as ds effs

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

logInteractor :: forall i as effs. Member (Logger Text) effs => as -> RunConfig -> i -> Sem effs as
logInteractor as (RunConfig t' _) i = log (t' <> " Hello from test " <> txt i) >> pure as


emptiParser :: a -> i -> as -> Sem effs a
emptiParser a _ _ = pure a

type LGEffs effs = Member (Logger Text) effs

test1 :: forall effs. LGEffs effs => MockTest MyInt Text MyText effs
test1 =
  RunnerBase.Test
    { config =
        TestConfig
          { header = "test1",
            address = TestAddress "test1",
            include = True
          },
      items = empti,
      interactor = logInteractor "Hello",
      parse = pure . MyText . txt
    }

test2 :: forall effs. LGEffs effs => MockTest MyInt Int MyText effs
test2 =
  Test
    { config =
        TestConfig
          { header = "test2",
            address = TestAddress "test2 address",
            include = True
          },
      items = empti,
      interactor = logInteractor 2,
      parse = pure . MyText . txt
    }

test3 :: forall effs. LGEffs effs => MockTest MyInt Int MyText effs
test3 =
  Test
    { config =
        TestConfig
          { header = "test3",
            address = TestAddress "test3 address",
            include = True
          },
      items = empti,
      interactor = logInteractor 5,
      parse = pure . MyText . txt
    }

test4 :: forall effs. LGEffs effs => MockTest MyText Text MyText effs
test4 =
  Test
    { config =
        TestConfig
          { header = "test4",
            address = TestAddress "test4 address",
            include = True
          },
      items = empti,
      interactor = logInteractor "4",
      parse = pure . MyText . txt
    }

test5 :: forall effs. LGEffs effs => MockTest MyInt Int MyText effs
test5 =
  Test
    { config =
        TestConfig
          { header = "test5",
            address = TestAddress "test 5",
            include = True
          },
      items = empti,
      interactor = logInteractor 5,
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

happySuite :: forall a effs. LGEffs effs => (forall i as ds. (Show i, Show as, Show ds, ToJSON as, ToJSON ds, ItemClass i ds) => MockTest i as ds effs -> a) -> SuiteItem effs [a]
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

runParams :: forall effs. MinEffs Text effs => RunParams Maybe MyText RunConfig TestConfig effs
runParams =
  RunParams
    { suite = happySuite,
      filters = filters',
      itemIds = Nothing,
      itemRunner = runItem,
      rc = RunConfig "Happy Run" True
    }

happyRun :: forall effs. MinEffs MyText effs => Sem effs ()
happyRun = mkRunSem runParams