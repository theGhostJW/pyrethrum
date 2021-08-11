module SemTest where

import Data.Aeson.TH
import Data.Aeson.Types
import Data.Yaml
import Polysemy
import Pyrelude as P hiding (Group, Item)
import Pyrelude.Test hiding (Group, Item)
import Runner as R
import RunnerBase (Test)
import TestFilter
import Check

data Include = In | Out deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions ''Include)

newtype RunConfig = RunConfig Include

data TestConfig = TestConfig
  { title :: Text,
    include :: Include
  }
  deriving (Show, Eq)

instance Config TestConfig

$(deriveJSON defaultOptions ''TestConfig)

--    e      tc        rc       hi i as ds effs
type MockTest hi i as ds effs = RunnerBase.Test Text TestConfig RunConfig hi (i ds) as ds effs

data Item ds = Item
  { id :: Int,
    title :: Text,
    val :: Int,
    checks :: Checks ds
  }
  deriving (Show, Generic)



data TextItem ds = TextItem
  { id :: Int,
    title :: Text,
    val :: Int,
    checks :: Checks ds
  }
  deriving (Show, Generic)


instance ToJSON (Item ds) where
  toEncoding = genericToEncoding defaultOptions

empti :: a -> [b]
empti = const ([] :: [b])

--                 as ->    rc     -> hi -> i -> Sem effs as
emptiInteractor :: as -> RunConfig -> hi -> i -> Sem effs as
emptiInteractor as _ _ _ = pure as

constParser :: a -> as -> Sem effs a
constParser a _ = pure a

test1Txt :: MockTest Text Item Text Int effs
test1Txt =
  Test
    { config =
        TestConfig
          { title = "test1",
            include = In
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = constParser 1
    }

test2Int :: MockTest Int Item Text Text effs
test2Int =
  Test
    { config =
        TestConfig
          { title = "test2",
            include = In
          },
      items = empti,
      interactor = emptiInteractor "Hi",
      parse = constParser "Hello"
    }

test3Bool :: MockTest Bool Item Text Int effs
test3Bool =
  Test
    { config =
        TestConfig
          { title = "test3",
            include = Out
          },
      items = empti,
      interactor = emptiInteractor "Hi",
      parse = constParser 2
    }

test4Txt :: MockTest Text Item Text Text effs
test4Txt =
  Test
    { config =
        TestConfig
          { title = "test4",
            include = In
          },
      items = empti,
      interactor = emptiInteractor "Hello",
      parse = constParser "hi"
    }

test5Int :: MockTest Int Item Int Int effs
test5Int =
  Test
    { config =
        TestConfig
          { title = "test5",
            include = Out
          },
      items = empti,
      interactor = emptiInteractor 1,
      parse = constParser 99
    }

includeFilter :: TestFilter RunConfig TestConfig
includeFilter =
  TestFilter
    { title = "test include must match run",
      predicate = \(RunConfig inc) _ tc -> include tc == inc
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