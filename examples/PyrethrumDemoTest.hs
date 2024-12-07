module PyrethrumDemoTest where

import Check (Checks, chk)
-- TODO Base should reexport all required types from core
import Core (After, Around, Before, Each, Once, ParseException, Thread)
import DSL.Internal.NodeLog (NodeLog (..), Path (..), UserLog (Info))
import DSL.OutEffect (out)
import Effectful (Eff)
import PyrethrumBase (
  Action,
  Depth (..),
  Fixture,
  HasLog,
  Hook (..),
  LogEffs,
  Node (..),
  RunConfig (..),
  Suite,
  FixtureConfig (..),
  FixtureConfig, Country (..), Environment (..), fxCfg, mkFull, mkFull', mkDirect', runConfig,
 )
import PyrethrumExtras (txt)
import CoreTypeFamilies (DataSource (Items))

{-
Note:: tried alternative with individual hook types but the results
were starting to look more complex than the original so abandonned.
-}

log :: (HasLog es) => Text -> Eff es ()
log = out . User . Info

logShow :: (HasLog es, Show a) => a -> Eff es ()
logShow = out . User . Info . txt

{- Demonstraits using partial effect
  type LogEffs a = forall es. (Out NodeLog :> es) => Eff es a

  Hook has all the effects of the application but will compile with
  an action that only requires a sublist of these effects
-}
logReturnInt :: () ->  LogEffs Int
logReturnInt _a = log "Returning One" >> pure 1

runSomethingToDoWithTestDepth :: Depth -> Action ()
runSomethingToDoWithTestDepth = logShow

demoOnceAfterHook :: Hook Once After () ()
demoOnceAfterHook =
  AfterHook
    { afterAction =  log "After all tests"
    }

intOnceHook :: Hook Once Before () Int
intOnceHook =
  BeforeHook'
    { depends = demoOnceAfterHook
    , action' = logReturnInt 
    }

addOnceIntHook :: Hook Once Before Int Int
addOnceIntHook =
  BeforeHook'
    { depends = intOnceHook
    , action' =
        \i -> do
          log $ "beforeAll' " <> txt i
          pure $ i + 1
    }

_intThreadHook :: Hook Thread Before () Int
_intThreadHook = BeforeHook $ do
  log "deriving meaning of life' "
  pure 42

data HookInfo = HookInfo
  { message :: Text
  , value :: Int
  }
  deriving (Show, Generic)

infoThreadHook :: Hook Thread Before Int HookInfo
infoThreadHook = BeforeHook' addOnceIntHook $ \i -> do
  log $ "beforeThread' " <> txt i
  pure $ HookInfo "Hello there" i

eachInfoAround :: Hook Each Around HookInfo Int
eachInfoAround =
  AroundHook'
    { aroundDepends = infoThreadHook
    , setup' = \hi -> do
        log "eachSetup"
        pure $ hi.value + 1
    , teardown' = \_i -> do
        log "eachTearDown"
        pure ()
    }

eachAfter :: Hook Each After Int Int
eachAfter =
  AfterHook'
    { afterDepends = eachInfoAround
    , afterAction' = do
        log "eachAfter"
        pure ()
    }

eachIntBefore :: Hook Each Before Int Int
eachIntBefore =
  BeforeHook'
    { depends = eachInfoAround
    , action' = \hi -> do
        log "eachSetup"
        pure $ hi + 1
    }

-- ############### Test the Lot ###################

config :: FixtureConfig
config = FxCfg "test" DeepRegression

test :: Fixture ()
test = mkFull config action parse dataSource

action :: Item -> Action ApState
action itm = do
  log $ txt itm
  pure $ ApState (itm.value + 1) $ txt itm.value

data ApState = ApState
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Read)

data VState = VState
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Read)

parse :: ApState -> Either ParseException VState
parse ApState{..} = pure VState{..}

data Item = Item
  { id :: Int
  , title :: Text
  , value :: Int
  , checks :: Checks VState
  }
  deriving (Show, Read)

dataSource :: RunConfig -> DataSource Item VState
dataSource =
  const $
    Items [ Item
        { id = 1
        , title = "test the value is one"
        , value = 2
        , checks = chk "test" ((== 1) . (.value))
        }
    ]

-- ############### Test the Lot Child ###################

config2 :: FixtureConfig
config2 = FxCfg "test" DeepRegression

test2 :: Fixture HookInfo
test2 = mkFull' config2 infoThreadHook action2 parse2 items2

action2 ::  HookInfo -> Item2 -> Action AS
-- action2 RunConfig{country, depth, environment} HookInfo{value = hookVal} itm = do
action2 HookInfo{value = hookVal} itm = do
  logShow itm
  RunConfig {country, depth, environment} <- runConfig
  when (country == AU )$
    log "Aus test"
  when (country == NZ) $
    log "NZ test"
  runSomethingToDoWithTestDepth depth
  unless (environment == Prod) $
    log "Completing payment with test credit card"
  pure $ AS (itm.value + 1 + hookVal) $ txt itm.value

parse2 :: AS -> Either ParseException VS
parse2 AS{..} = pure VS{..}

data AS = AS
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Read)

data VS = VS
  { value :: Int
  , valTxt :: Text
  }
  deriving (Show, Read)

data Item2 = Item2
  { id :: Int
  , title :: Text
  , value :: Int
  , checks :: Checks VS
  }
  deriving (Show, Read)

items2 :: RunConfig -> DataSource Item2 VS
items2 rc =
  Items $ filter
    (\i -> rc.depth == Regression || i.id < 10)
    [ Item2
        { id = 1
        , title = "test the value is one"
        , value = 2
        , checks =
            chk "test" ((== 1) . (.value))
              <> chk "test2" (\VS{..} -> value < 10)
              <> chk "test3" (\ds -> ds.value < 10)
        }
    ]

-- TODO: precompiler / teplateHaskell

-- ############### Test the Lot (Record) ###################

test3 :: Fixture Int
test3 =
  mkFull'
    (FxCfg "test" DeepRegression)
    eachIntBefore
    (
    \hkInt itm -> do
        log $ txt itm
        pure $ AS (itm.value + 1 + hkInt) $ txt itm.value
        )
    (\AS{..} -> pure VS{..})
    (
    const .
         Items $ [ Item2
              { id = 1
              , title = "test the value is one"
              , value = 2
              , checks = chk "test" ((== 1) . (.value))
              }
          ]
    )


-- ############### Test Direct (Record) ###################
test4 :: Fixture Int
test4 =
  mkDirect'
    (FxCfg "test" DeepRegression)
    eachAfter
    (\_hi itm -> do
        log $ txt itm
        pure $ VS (itm.value + 1) $ txt itm.value
    )
    (
    const .
          Items $ [ Item2
              { id = 1
              , title = "test the value is one"
              , value = 2
              , checks = chk "test" ((== 1) . (.value))
              }
          ]
    )
  

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

-- ############### Demo Default Configs ###################

cfg :: FixtureConfig
cfg = fxCfg "test"

-- ambiguous record update error - should work
-- after 9.8.1
-- cfg2 :: FixtureConfig
-- cfg2 = (FixtureConfig "test") {
--   depth = Regression
-- }

-- ############### Suite ###################
-- this will be generated

suite :: Suite
suite =
  [ Fixture (NodePath "module" "testName") test
  , Hook
      { path = NodePath "module" "name"
      , hook = demoOnceAfterHook
      , subNodes =
          [ Hook
              { path = NodePath "module" "name"
              , hook = intOnceHook
              , subNodes =
                  [ Fixture (NodePath "module" "testName") test4
                  , Hook
                      { path = NodePath "module" "name"
                      , hook = addOnceIntHook
                      , subNodes =
                          [ Hook
                              { path = NodePath "module" "name"
                              , hook = infoThreadHook
                              , subNodes =
                                  [ Fixture (NodePath "module" "testName") test2
                                  , Hook
                                      { path = NodePath "module" "name"
                                      , hook = eachInfoAround
                                      , subNodes =
                                          [ Fixture (NodePath "module" "testName") test3
                                          , Hook
                                              { path = NodePath "module" "name"
                                              , hook = eachAfter
                                              , subNodes =
                                                  [ Fixture (NodePath "module" "testName") test4
                                                  , Fixture (NodePath "module" "testName") test3
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
    stubFilter (takes a predicate and stubs all dataSource that match it)

-- ToDO webdriveIO protocol
--- https://github.com/webdriverio/webdriverio/blob/main/packages/wdio-protocols/src/protocols/gecko.ts
-}
