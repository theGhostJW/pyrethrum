module MockSuite where

import AuxFiles ()
import Common ()
import DSL.Interpreter (MinEffs, executeForTest, minInterpret)
import DSL.LogProtocol (LogProtocolBase)
import DSL.LogProtocol.PrettyPrint ()
import DSL.Logger
import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    defaultOptions,
    genericToEncoding,
  )
import Data.Aeson.TH
import ItemRunners
import LogTransformation.Common ()
import Polysemy
import Pyrelude
import qualified Pyrelude as P
import Pyrelude.IO ()
import Pyrelude.Test as T ()
import RunElementClasses
  ( ItemClass (checkList, identifier, thenClause, whenClause),
    RunConfigClass,
    TestAddress (TestAddress),
    TestConfigClass (..),
    Titled (..),
  )
import Runner as R
  ( FrameworkError,
    HookLocation (BeforeAll, BeforeEach),
    RunParams (..),
    SuiteItem (Group, Hook, Tests),
    Test (Test, config, interactor, items, parse),
    mkRunSem,
  )
import qualified RunnerBase
import TestFilter (TestFilter (..))

type AppError = FrameworkError Text

type LogProtocol = LogProtocolBase AppError

type LogProtocolTextError = LogProtocolBase Text

data RunConfig = RunConfig
  { cfgHeader :: Text,
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

instance ItemClass Int MyText where
  identifier i = i
  whenClause _ = "pre"
  thenClause _ = "post"
  checkList = mempty

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

logInteractor :: forall i as effs. (Member (Logger Text) effs, Show i) => as -> RunConfig -> i -> Sem effs as
logInteractor as (RunConfig t' _) i = log (t' <> " - Hello from item " <> txt i) >> pure as

emptiParser :: a -> i -> as -> Sem effs a
emptiParser a _ _ = pure a

type Lgrffs effs = Member (Logger Text) effs

type DemoEffs effs = MinEffs Text effs

test1 :: forall effs. Lgrffs effs => MockTest Int Text MyText effs
test1 =
  RunnerBase.Test
    { config =
        TestConfig
          { header = "test1",
            address = TestAddress "test1",
            include = True
          },
      items = const [1, 2],
      interactor = logInteractor "test1: ",
      parse = pure . MyText . txt
    }

test2 :: forall effs. Lgrffs effs => MockTest MyInt Int MyText effs
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

test3 :: forall effs. Lgrffs effs => MockTest MyInt Int MyText effs
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

test4 :: forall effs. Lgrffs effs => MockTest MyText Text MyText effs
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

test5 :: forall effs. Lgrffs effs => MockTest MyInt Int MyText effs
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

demoSuit :: SuiteItem effs [Text]
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
            [ "test 4",
              "test 5"
            ]
        ],
      R.Group
        "Empty Group"
        [ Tests []
        ]
    ]

happySuite :: forall a effs. Lgrffs effs => (forall i as ds. (Show i, Show as, Show ds, ToJSON as, ToJSON ds, ToJSON i, ItemClass i ds) => MockTest i as ds effs -> a) -> SuiteItem effs [a]
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
        [ Tests []
        ]
    ]

hookSuite ::
  forall a effs.
  Lgrffs effs =>
  -- | test runner
  (forall i as ds. (Show i, Show as, Show ds, ToJSON as, ToJSON ds, ToJSON i, ItemClass i ds) => MockTest i as ds effs -> a) ->
  SuiteItem effs [a]
hookSuite r =
  R.Group
    "Hook Suite"
    [ Hook
        BeforeEach
        (log "Before Each Hook")
        [ Hook
            BeforeAll
            (log "Before All Hook")
            [Tests [r test1]]
        ]
    ]

runParams :: forall effs. DemoEffs effs => RunParams Maybe Text RunConfig TestConfig effs
runParams =
  RunParams
    { suite = happySuite,
      filters = filters',
      itemIds = Nothing,
      itemRunner = runItem,
      rc = RunConfig "Happy Suite" True
    }

happyRun :: forall effs. DemoEffs effs => Sem effs ()
happyRun = mkRunSem runParams

hookParams :: forall effs. DemoEffs effs => RunParams Maybe Text RunConfig TestConfig effs
hookParams =
  RunParams
    { suite = hookSuite,
      filters = [],
      itemIds = Nothing,
      itemRunner = runItem,
      rc = RunConfig "Hook Suite" True
    }

hookRun :: forall effs. DemoEffs effs => Sem effs ()
hookRun = mkRunSem hookParams
