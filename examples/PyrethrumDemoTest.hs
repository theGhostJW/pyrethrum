module PyrethrumDemoTest where

import Core (Checks, EachBefore, EachResource, OnceBefore, OnceParam, ParseException, Path (..), ThreadBefore, chk)
import DSL.Internal.ApEvent (ApEvent (..), ULog (Log))
import DSL.Out (Out, out)
import Effectful (Eff, IOE, (:>))
import PyrethrumDemoProject (
  Action,
  Depth (DeepRegression),
  Hook (..),
  RunConfig (..),
  Suite,
  SuiteElement (..),
  Test (..),
  TestConfig (..),
 )
import PyrethrumExtras (txt)

log :: (Out ApEvent :> es) => Text -> Eff es ()
log = out . User . Log

intOnceHook :: Hook OnceBefore () Int
intOnceHook =
  OnceBefore
    { onceAction = \rc -> pure 1
    }

addOnceIntHook :: Hook OnceBefore Int Int
addOnceIntHook =
  OnceBefore'
    { -- onceParent = intThreadHook
      onceParent = intOnceHook
    , onceAction' =
        \i rc -> do
          log $ "beforeAll' " <> txt i
          pure $ i + 1
    }

intThreadHook :: Hook ThreadBefore () Int
intThreadHook = ThreadBefore $ \rc -> do
  log "deriving meaning of life' "
  pure 42

data HookInfo = HookInfo
  { message :: Text
  , value :: Int
  }
  deriving (Show, Generic)

infoThreadHook :: Hook ThreadBefore Int HookInfo
infoThreadHook = ThreadBefore' addOnceIntHook $ \i rc -> do
  log $ "beforeThread' " <> txt i
  pure $ HookInfo "Hello there" i

eachInfoResource :: Hook EachResource HookInfo Int
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

eachIntBefore :: Hook EachBefore Int Int
eachIntBefore =
  EachBefore'
    { eachParent = eachInfoResource
    , eachAction' = \hi rc -> do
        log "eachSetup"
        pure $ hi + 1
    }


-- ############### Test the Lot ###################
test :: Test ()
test =
  Full config action parse items

config :: TestConfig
config = TestConfig "test" 1 DeepRegression

data ApState = ApState
  { value :: Int
  , valTxt :: Text
  }

action :: RunConfig -> Item -> Action ApState
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
test2 :: Test HookInfo
test2 =
  Full' infoThreadHook config2 action2 parse2 items2

config2 :: TestConfig
config2 = TestConfig "test" 1 DeepRegression

action2 :: HookInfo -> RunConfig -> Item2 -> Action AS
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
test3 :: Test Int
test3 =
  Full'
    { parent = eachIntBefore
    , config' = TestConfig "test" 1 DeepRegression
    , childAction = \i rc itm -> do
        log $ txt itm
        pure $ AS (itm.value + 1 + i) $ txt itm.value
    , parse' = \AS{..} -> pure DS{..}
    , items' =
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
test4 :: Test ()
test4 =
  NoParse
    { config = TestConfig "test" 1 DeepRegression
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
test5 :: Test ()
test5 =
  Single
    { config = TestConfig "test" 1 DeepRegression
    , singleAction = \rc -> do
        log $ "RunConfig is: " <> rc.title
        pure
          $ DS
            { value = 1
            , valTxt = rc.title
            }
    , checks = chk "the value must be 1" ((== 1) . (.value))
    }

-- ############### Suite ###################
-- this will be generated be generated

suite :: Suite
suite =
  [ Test (Path "module" "testName") test
  , Test (Path "module" "testName") test4
  , Test (Path "module" "testName") test5
  , Hook
      { path = Path "module" "name"
      , hook = intOnceHook
      , subNodes =
          [ Hook
              { path = Path "module" "name"
              , hook = addOnceIntHook
              , subNodes =
                  [ Hook
                      { path = Path "module" "name"
                      , hook = addOnceIntHook
                      , subNodes =
                          [ Hook
                              { path = Path "module" "name"
                              , hook = infoThreadHook
                              , subNodes =
                                  [ Test (Path "module" "testName") test2
                                  , Hook
                                      { path = Path "module" "name"
                                      , hook = eachInfoResource
                                      , subNodes =
                                          [ Hook
                                              { path = Path "module" "name"
                                              , hook = eachIntBefore
                                              , subNodes =
                                                  [Test (Path "module" "testName") test3]
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
      }
  ]

{-
-- TODO: review bracket

-- TODO : stubs:
  - reinstate runner => Actions
     - indivdual tests ?
  - PreNode2 - what format
  - execution Exetre

-- TODO : stubs:

    stubOnly (only function - takes an item and adds "DEBUG ONLY" to the title)",
    get rid of ids ?? dubious
    stubRaw,
    stubHash (make a hash of props other than id and title: aDfG165Er7 - 10 letter hash - module + item first 4 letters is module, next 6 is item),
    stubAll,
    stubFilter (takes a predicate and stubs all items that match it)

-- ToDO webdriveIO protocol
--- https://github.com/webdriverio/webdriverio/blob/main/packages/wdio-protocols/src/protocols/gecko.ts
-}
