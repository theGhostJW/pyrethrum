module PyrethrumDemoTest where

import Check (Checks, chk)
-- import Core (After, Around, Before, Each, Once, ParseException, Thread) 
-- import Core hiding (Fixture(..), Hook(..)) 
import Core
import DSL.Internal.ApEvent (ApEvent (..), Path (..), ULog (Log))
import DSL.Out (Out, out)
import Effectful (Eff, (:>))
import PyrethrumDemoProject (
  Action,
  Depth (..),
  TFixture (..),
  THook (..),
  LogEffs,
  -- Node (..),
  RunConfig (..),
  -- Suite,
  TestConfig (..),
  testConfig,
 )
import PyrethrumExtras (txt)

{-
Note:: tried alternative with individual hook types but the results
were starting to look more complex than the original so abandonned.
-}

log :: (Out ApEvent :> es) => Text -> Eff es ()
log = out . User . Log

{- Demonstraits using partial effect
  type LogEffs a = forall es. (Out ApEvent :> es) => Eff es a

  Hook has all the effects of the application but will compile with
  an action that only requires a sublist of these effects
-}
simpleLog :: RunConfig -> LogEffs Int
simpleLog _ = pure 1

intOnceHook :: THook Once () Int
intOnceHook =
  Before
    { action = simpleLog
    }

addOnceIntHook :: THook Once Int Int
addOnceIntHook =
  Before'
    { depends = intOnceHook
    , action' =
        \_rc i -> do
          log $ "beforeAll' " <> txt i
          pure $ i + 1
    }

_intThreadHook :: THook Thread () Int
_intThreadHook = Before $ \_rc -> do
  log "deriving meaning of life' "
  pure 42

data HookInfo = HookInfo
  { message :: Text
  , value :: Int
  }
  deriving (Show, Generic)

infoThreadHook :: THook Thread Int HookInfo
infoThreadHook = Before' addOnceIntHook $ \_rc i -> do
  log $ "beforeThread' " <> txt i
  pure $ HookInfo "Hello there" i

eachInfoAround :: THook Each HookInfo Int
eachInfoAround =
  Around'
    { depends = infoThreadHook
    , setup' = \_rc hi -> do
        log "eachSetup"
        pure $ hi.value + 1
    , teardown' = \_rc _i -> do
        log "eachTearDown"
        pure ()
    }

eachAfter :: THook Each Int Int
eachAfter =
  After'
    { afterDepends = eachInfoAround
    , afterAction' = \_rc -> do
        log "eachAfter"
        pure ()
    }

eachIntBefore :: THook Each Int Int
eachIntBefore =
  Before'
    { depends = eachInfoAround
    , action' = \_rc hi -> do
        log "eachSetup"
        pure $ hi + 1
    }

-- ############### Test the Lot ###################

config :: TestConfig
config = TestConfig "test" DeepRegression

test :: TFixture ()
test = Full config action parse items

action :: RunConfig -> ItemData -> Action ApState
action _expectedrc itm = do
  log $ txt itm
  pure $ ApState (itm.value + 1) $ txt itm.value

data ApState = ApState
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Read)

data DState = DState
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Read)

parse :: ApState -> Either ParseException DState
parse ApState{..} = pure DState{..}

data ItemData = ItemData
  { id :: Int
  , title :: Text
  , value :: Int
  , checks :: Checks DState
  }
  deriving (Show, Read)

items :: RunConfig -> [ItemData]
items =
  const
    [ ItemData
        { id = 1
        , title = "test the value is one"
        , value = 2
        , checks = chk "test" ((== 1) . (.value))
        }
    ]

-- ############### Test the Lot Child ###################
config2 :: TestConfig
config2 = TestConfig "test" DeepRegression

test2 :: TFixture HookInfo
test2 = Full' config2 infoThreadHook action2 parse2 items2

action2 :: RunConfig -> HookInfo -> Item2 -> Action AS
action2 _rc HookInfo{value = hookVal} itm = do
  log $ txt itm
  pure $ AS (itm.value + 1 + hookVal) $ txt itm.value

parse2 :: AS -> Either ParseException DS
parse2 AS{..} = pure DS{..}

data AS = AS
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Read)

data DS = DS
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Read)

data Item2 = Item2
  { id :: Int
  , title :: Text
  , value :: Int
  , checks :: Checks DS
  }
  deriving (Show, Read)

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

-- TODO: precompiler / teplateHaskell

-- ############### Test the Lot (Record) ###################

test3 :: TFixture Int
test3 =
  Full'
    { depends = eachIntBefore
    , config' = TestConfig "test" DeepRegression
    , action' = \_rc hkInt itm -> do
        log $ txt itm
        pure $ AS (itm.value + 1 + hkInt) $ txt itm.value
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

-- ############### Test Direct (Record) ###################
test4 :: TFixture Int
test4 =
  Direct'
    { config' = TestConfig "test" DeepRegression
    , depends = eachAfter
    , action' = \_rc _hi itm -> do
        log $ txt itm
        pure $ DS (itm.value + 1) $ txt itm.value
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


-- ############### Construct Tests ###################
-- this will be generated either by implmenting deriving,
-- check out DeriveAnyClass or template haskell
-- could also look into creating un unconstrained data types
-- all members of a convertable typeclass (specialize??)
-- and converting to a true test fixture at the bottom of the file
-- after deriveJson and ItemData
-- TODO: precompiler template haskell
-- need to check error messages carefully
-- finalise templatehaskell vs deriving for these classes

-- ############### Demo Default Configs ###################

cfg :: TestConfig
cfg = testConfig "test"

-- ambiguous record update error - should work
-- after 9.8.1
-- cfg2 :: TestConfig
-- cfg2 = (testConfig "test") {
--   depth = Regression
-- }

-- ############### Suite ###################
-- this will be generated
{-
suite :: Suite
suite =
  [ Test (NodePath "module" "testName") test
  , Hook
      { path = NodePath "module" "name"
      , hook = intOnceHook
      , subNodes =
          [ Test (NodePath "module" "testName") test4
          , Hook
              { path = NodePath "module" "name"
              , hook = addOnceIntHook
              , subNodes =
                  [ Hook
                      { path = NodePath "module" "name"
                      , hook = infoThreadHook
                      , subNodes =
                          [ Test (NodePath "module" "testName") test2
                          , Hook
                              { path = NodePath "module" "name"
                              , hook = eachInfoAround
                              , subNodes =
                                  [ Test (NodePath "module" "testName") test3
                                  , Hook
                                      { path = NodePath "module" "name"
                                      , hook = eachAfter
                                      , subNodes =
                                          [ Test (NodePath "module" "testName") test4,
                                            Test (NodePath "module" "testName") test3
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
-}

{-
-- TODO: test documenter that returns a handle from onceHook
      - research lazy vs strict TVar - may need to run differently doc and exe
      - aftr depends on before
      - after depends on around

-- TODO: review bracket

-- TODO : stubs:
  - reinstate runner => Actions
     - indivdual tests ?

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
