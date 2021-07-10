module SemTest where

import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import Polysemy
import Pyrelude as P
import Pyrelude.Test hiding (Group)
import Runner as R
import RunnerBase (Test)
import TestFilter

data Include = In | Out deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''Include)

newtype RunConfig = RunConfig Include

data TestConfig = TestConfig
  { header :: Text,
    address :: TestAddress,
    include :: Include
  }
  deriving (Show, Eq)

instance TestConfigClass TestConfig where
  moduleAddress = address

instance Titled TestConfig where
  title = header

$(deriveJSON defaultOptions ''TestConfig)

--    e      tc        rc       hi i as ds effs
type MockTest hi i as ds effs = RunnerBase.Test Text TestConfig RunConfig hi i as ds effs

newtype MyInt = MyInt Int deriving (Show, Generic)

newtype MyText = MyText Text deriving (Show, Generic, ToJSON)

instance ItemClass MyInt MyInt where
  identifier _ = -999
  whenClause _ = "pre"
  thenClause _ = "post"
  checkList = mempty

instance ItemClass MyText MyText where
  identifier _ = -999
  whenClause _ = "pre"
  thenClause _ = "post"
  checkList = mempty

instance ToJSON MyInt where
  toEncoding = genericToEncoding defaultOptions

empti :: a -> [b]
empti = const ([] :: [b])

--                 as ->    rc     -> hi -> i -> Sem effs as
emptiInteractor :: as -> RunConfig -> hi -> i -> Sem effs as
emptiInteractor as _ _ _ = pure as

emptiParser :: a -> as -> Sem effs a
emptiParser a _ = pure a

test1Txt :: MockTest Text MyInt Text MyInt effs
test1Txt =
  Test
    { config =
        TestConfig
          { header = "test1",
            address = TestAddress "test1",
            include = In
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = emptiParser (MyInt 1)
    }

test2Int :: MockTest Int MyInt MyInt MyInt effs
test2Int =
  Test
    { config =
        TestConfig
          { header = "test2",
            address = TestAddress "test2 address",
            include = In
          },
      items = empti,
      interactor = emptiInteractor (MyInt 1),
      parse = pure
    }

test3Bool :: MockTest Bool MyInt MyInt MyInt effs
test3Bool =
  Test
    { config =
        TestConfig
          { header = "test3",
            address = TestAddress "test3 address",
            include = Out
          },
      items = empti,
      interactor = emptiInteractor (MyInt 3),
      parse = pure
    }

test4Txt :: MockTest Text Text Text Text effs
test4Txt =
  Test
    { config =
        TestConfig
          { header = "test4",
            address = TestAddress "test4 address",
            include = In
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = pure
    }

test5Int :: MockTest Int MyInt MyInt MyInt effs
test5Int =
  Test
    { config =
        TestConfig
          { header = "test5",
            address = TestAddress "test5 address",
            include = Out
          },
      items = empti,
      interactor = emptiInteractor (MyInt 1),
      parse = pure
    }

includeFilter :: TestFilter RunConfig TestConfig
includeFilter =
  TestFilter
    { title = "test include must match run",
      predicate = \(RunConfig inc) tc -> include tc == inc
    }

filters' :: [TestFilter RunConfig TestConfig]
filters' = [includeFilter]

-- mockSuite :: forall effs a. (forall hi i as ds. (Show i, Show as, Show ds) => hi -> MockTest hi i as ds effs -> a) -> SuiteItem () effs [a]
-- mockSuite r =
--   R.Group
--     "Filter TestSuite"
--     [ BeforeHook
--         { title = "Before All",
--           cardinality = ExeOnce,
--           bHook = pure "hello",
--           bhElms =
--             [ \t ->
--                 Tests
--                   [ r t test1Txt,
--                     r t test4Txt
--                   ],
--               const
--                 R.Group
--                   { title = "Empty Group",
--                     gElms =
--                       [ Tests []
--                       ]
--                   }
--             ]
--         },
--       R.Group
--         { title = "Empty Group",
--           gElms =
--             [ BeforeHook
--                 { title = "Int Group",
--                   cardinality = ExeForEach,
--                   bHook = pure 23,
--                   bhElms =
--                     [ \t ->
--                         AfterHook
--                           { title = "After Exch Int",
--                             cardinality = ExeForEach,
--                             aHook = t == 23 ? pure () $ pure (),
--                             ahElms =
--                               [ \i ->
--                                   Tests
--                                     [ r i test5Int,
--                                       r i test2Int
--                                     ]
--                               ]
--                           }
--                     ]
--                 }
--             ]
--         }
--         -- ,

        -- R.Group "Sub Group" [
        --   Tests [
        --     r test4,
        --     r test5
        --   ]
        -- ],
    -- ]

-- unit_test_filter_expect_empty