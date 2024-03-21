module PyrethrumDemoTest where

import Check (Checks, chk)
import Core (Each, Once, ParseException, Thread, Before, Around, After)
import DSL.Internal.ApEvent (ApEvent (..), Path (..), ULog (Log))
import DSL.Out (Out, out)
import Data.Aeson.TH
import Effectful (Eff, (:>))
import Effectful.Error.Static qualified as E
import PyrethrumDemoProject (
  Action,
  Depth (..),
  Hook (..),
  RunConfig (..),
  Suite,
  Node (..),
  Fixture (..),
  TestConfig (..), testConfig,
 )
import PyrethrumExtras (txt)

{-
Note:: tried alternative with individual hook types but the results 
were starting to look more complex than the original so abandonned.
-}

log :: (Out ApEvent :> es) => Text -> Eff es ()
log = out . User . Log

intOnceHook :: Hook Once Before () Int
intOnceHook =
  BeforeHook
    { action = \_rc -> pure 1
    }

addOnceIntHook :: Hook Once Before Int Int
addOnceIntHook =
  BeforeHook'
    { depends = intOnceHook
    , action' =
        \_rc i -> do
          log $ "beforeAll' " <> txt i
          pure $ i + 1
    }

_intThreadHook :: Hook Thread Before () Int
_intThreadHook = BeforeHook $ \_rc -> do
  log "deriving meaning of life' "
  pure 42

data HookInfo = HookInfo
  { message :: Text
  , value :: Int
  }
  deriving (Show, Generic)

infoThreadHook :: Hook Thread Before Int HookInfo
infoThreadHook = BeforeHook' addOnceIntHook $ \_rc i -> do
  log $ "beforeThread' " <> txt i
  pure $ HookInfo "Hello there" i

eachInfoAround :: Hook Each Around HookInfo Int
eachInfoAround =
  AroundHook'
    { aroundDepends = infoThreadHook
    , setup' = \_rc hi -> do
        log "eachSetup"
        pure $ hi.value + 1
    , teardown' = \_rc _i -> do
        log "eachTearDown"
        pure ()
    }

eachAfter :: Hook Each After Int Int
eachAfter =
  After'
    { afterDepends = eachInfoAround
    , afterAction' = \_rc -> do
        log "eachAfter"
        pure ()
    }

eachIntBefore :: Hook Each Before Int Int
eachIntBefore =
  BeforeHook'
    { depends = eachInfoAround
    , action' = \_rc hi -> do
        log "eachSetup"
        pure $ hi + 1
    }

type Failable a = Eff '[E.Error ParseException] a

-- ############### Test the Lot ###################

config :: TestConfig
config = TestConfig "test" DeepRegression

data ApState = ApState
  { value :: Int
  , valTxt :: Text
  }

action :: RunConfig -> Item -> Action ApState
action _expectedrc itm = do
  log $ txt itm
  pure $ ApState (itm.value + 1) $ txt itm.value

data DState = DState
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Generic)

parse :: ApState -> Failable DState
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

config2 :: TestConfig
config2 = TestConfig "test" DeepRegression

action2 :: RunConfig -> HookInfo -> Item2 -> Action AS
action2 _rc HookInfo{value = hookVal} itm = do
  log $ txt itm
  pure $ AS (itm.value + 1 + hookVal) $ txt itm.value

parse2 :: AS -> Failable DS
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
  deriving (Show)

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


$(deriveToJSON defaultOptions ''DS)
$(deriveToJSON defaultOptions ''AS)
$(deriveToJSON defaultOptions ''Item2)

test2 :: Fixture HookInfo
test2 = Full' infoThreadHook config2 action2 parse2 items2

-- TODO: precompiler / teplateHaskell

-- ############### Test the Lot (Record) ###################

test3 :: Fixture Int
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

-- ############### Test NoParse (Record) ###################
test4 :: Fixture Int
test4 =
  NoParse'
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

-- ############### Test Single (Record) ###################
test5 :: Fixture Int
test5 =
  Single'
    { config' = TestConfig "test" DeepRegression
    , depends = eachAfter
    , singleAction' = \rc _hi -> do
        log $ "RunConfig is: " <> rc.title
        pure
          $ DS
            { value = 1
            , valTxt = rc.title
            }
    , checks' = chk "the value must be 1" ((== 1) . (.value))
    }

-- ############### Construct Tests ###################
-- this will be generated either by implmenting deriving,
-- check out DeriveAnyClass or template haskell
-- could also look into creating un unconstrained data types
-- all members of a convertable typeclass (specialize??)
-- and converting to a true test fixture at the bottom of the file
-- after deriveJson and Item
-- TODO: precompiler template haskell
-- need to check error messages carefully
-- finalise templatehaskell vs deriving for these classes

-- $(deriveTest defaultOptions ''Item)
$(deriveToJSON defaultOptions ''DState)
$(deriveToJSON defaultOptions ''ApState)
$(deriveToJSON defaultOptions ''Item)

test :: Fixture ()
test =
  Full config action parse items

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

suite :: Suite
suite =
  [ Test (SuiteElmPath "module" "testName") test
  , Hook
      { path = SuiteElmPath "module" "name"
      , hook = intOnceHook
      , subNodes =
          [ Test (SuiteElmPath "module" "testName") test4
          , Test (SuiteElmPath "module" "testName") test5
          , Hook
              { path = SuiteElmPath "module" "name"
              , hook = addOnceIntHook
              , subNodes =
                  [ Hook
                      { path = SuiteElmPath "module" "name"
                      , hook = infoThreadHook
                      , subNodes =
                          [ Test (SuiteElmPath "module" "testName") test2
                          , Hook
                              { path = SuiteElmPath "module" "name"
                              , hook = eachInfoAround
                              , subNodes =
                                  [ Test (SuiteElmPath "module" "testName") test3
                                  , Hook
                                      { path = SuiteElmPath "module" "name"
                                      , hook = eachAfter
                                      , subNodes =
                                          [ Test (SuiteElmPath "module" "testName") test4
                                          , Test (SuiteElmPath "module" "testName") test5
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
