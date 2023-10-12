module PyrethrumDemoTest where

import Core (Checks, EachParent, OnceParam, OnceParent, ParseException, ThreadParent, chk)
import DSL.Internal.ApEvent (ApEvent (Log))
import DSL.Out (Out, out)
import Effectful (Eff, IOE, (:>))
import PyrethrumDemoPrj (
  Depth (DeepRegression),
  Fixture (..),
  RunConfig (..),
  Suite,
  Test (..),
  TestConfig (TestConfig),
  TestFixture,
 )
import PyrethrumExtras (txt)

log :: (Out ApEvent :> es) => Text -> Eff es ()
log = out . Log

log2 :: (Out ApEvent :> es) => Text -> Eff es ()
log2 = out . Log

hkConstructDemo :: Int -> Fixture OnceParent Int
hkConstructDemo _ = intOnceHook

intOnceHook :: Fixture OnceParent Int
intOnceHook =
  OnceBefore
    { onceAction = \rc -> pure 1
    }

addOnceIntHook :: Fixture OnceParent Int
addOnceIntHook =
  OnceBefore'
    { onceParent = intOnceHook
    , onceAction' =
        \i rc -> do
          log $ "beforeAll' " <> txt i
          pure $ i + 1
    }

data HookInfo = HookInfo
  { message :: Text
  , value :: Int
  }
  deriving (Show, Generic)

infoThreadHook :: Fixture ThreadParent HookInfo
infoThreadHook = ThreadBefore' addOnceIntHook $ \i rc -> do
  log $ "beforeThread' " <> txt i
  pure $ HookInfo "Hello there" i

eachInfoResource :: Fixture EachParent HookInfo
eachInfoResource =
  EachResource'
    { eachResourceParent = infoThreadHook
    , eachSetup' = \hi rc -> do
        log "eachSetup"
        pure $ hi.value + 1
    , eachTearDown' = \i -> do
        log "eachTearDown"
        pure ()
    }

eachIntBefore :: Fixture EachParent Int
eachIntBefore =
  EachBefore'
    { eachParent = eachInfoResource
    , eachAction' = \hi rc -> do
        log "eachSetup"
        pure $ hi.value + 1
    }

-- ############### Test the Lot ###################
test :: TestFixture
test =
  Test $ Full config action parse items

config :: TestConfig
config = TestConfig "test" DeepRegression

data ApState = ApState
  { value :: Int
  , valTxt :: Text
  }

action :: RunConfig -> Item -> Suite ApState
action rc itm = do
  log $ txt itm
  pure $ ApState (itm.value + 1) $ txt itm.value

data DState = DState
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Generic)

parse :: ApState -> Either ParseException DState
parse ApState{..} = pure DState{..}

data Item = Item
  { id :: Int
  , title :: Text
  , value :: Int
  , checks :: Checks DState
  }
  deriving (Show, Generic)

items :: RunConfig -> [Item]
items =
  const
    [ Item
        { id = 1
        , title = "test the value is one"
        , value = 2
        , checks = chk "test" ((== 1) . (.value))
        }
    ]

-- ############### Test the Lot Child ###################
test2 :: TestFixture
test2 =
  Test $ Full' infoThreadHook config2 action2 parse2 items2

config2 :: TestConfig
config2 = TestConfig "test" DeepRegression

action2 :: HookInfo -> RunConfig -> Item2 -> Suite AS
action2 HookInfo{value = hookVal} rc itm = do
  log $ txt itm
  pure $ AS (itm.value + 1 + hookVal) $ txt itm.value

parse2 :: AS -> Either ParseException DS
parse2 AS{..} = pure DS{..}

data AS = AS
  { value :: Int
  , valTxt :: Text
  }

data DS = DS
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Generic)

data Item2 = Item2
  { id :: Int
  , title :: Text
  , value :: Int
  , checks :: Checks DS
  }
  deriving (Show, Generic)

items2 :: RunConfig -> [Item2]
items2 =
  const
    [ Item2
        { id = 1
        , title = "test the value is one"
        , value = 2
        , checks =
            chk "test" ((== 1) . (.value))
              <> chk "test2" (\DS{..} -> value < 10)
              <> chk "test3" (\ds -> ds.value < 10)
        }
    ]

-- ############### Test the Lot (Record) ###################

test3 :: TestFixture
test3 =
  Test
    $ Full'
      { parent = eachIntBefore
      , config = TestConfig "test" DeepRegression
      , childAction = \i rc itm -> do
          log $ txt itm
          pure $ AS (itm.value + 1 + i) $ txt itm.value
      , parse = \AS{..} -> pure DS{..}
      , items =
          const
            [ Item2
                { id = 1
                , title = "test the value is one"
                , value = 2
                , checks = chk "test" ((== 1) . (.value))
                }
            ]
      }

-- ############### Test NoParse (Record) ###################
test4 :: TestFixture
test4 =
  Test
    $ NoParse
      { config = TestConfig "test" DeepRegression
      , action = \rc itm -> do
          log $ txt itm
          pure $ DS (itm.value + 1) $ txt itm.value
      , items =
          const
            [ Item2
                { id = 1
                , title = "test the value is one"
                , value = 2
                , checks = chk "test" ((== 1) . (.value))
                }
            ]
      }

-- ############### Test Single (Record) ###################

test5 :: TestFixture
test5 =
  Test
    $ Single
      { config = TestConfig "test" DeepRegression
      , singleAction = \rc -> do
          log $ "RunConfig is: " <> rc.title
          pure
            $ DS
              { value = 1
              , valTxt = rc.title
              }
      , checks = chk "the value must be 1" ((== 1) . (.value))
      }



{-
  h 
   h 
    t
    t 
  t

-- TODO : stubs:

    stubOnly (only function - takes an item and adds "DEBUG ONLY" to the title)",
    get rid of ids
    stubRaw,
    stubHash (make a hash of props other than id and title: aDfG165Er7 - 10 letter hash - module + item first 4 letters is module, next 6 is item),
    stubAll,
    stubFilter (takes a predicate and stubs all items that match it)
-}
