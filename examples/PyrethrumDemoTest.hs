module PyrethrumDemoTest where

import Check (Checks, chk)
import Core (After, Around, Before, Each, ExeParams (..), Logger (..), Once, ParseException, Thread, runNode)
import DSL.Internal.ApEvent (ApEvent (..), FLog (Step), Path (..), ULog (Log))
import DSL.Out (out)
import Effectful (Eff)
import IOEffectDemo (ioRun)
import Internal.RunTimeLogging (testLogControls)
import Internal.SuiteRuntime (ThreadCount (ThreadCount), execute)
import PyrethrumBase (
    Depth (..),
    Fixture (..),
    HasLog,
    Hook (..),
    LogEffs,
    Node (..),
    RunConfig (..),
    Suite,
    TestConfig (..),
    mkNode,
    testConfig,
 )
import PyrethrumConfigTypes (Country (..), Environment (Prod))
import PyrethrumExtras (txt, uu)
import UnliftIO.STM (TQueue, newTQueueIO, tryReadTQueue, writeTQueue)

{-
Note:: tried alternative with individual hook types but the results
were starting to look more complex than the original so abandonned.
-}

log :: (Logger m) => Text -> m ()
log = fLog . Step

logShow :: (Show a) => a -> IO ()
logShow = log . txt

{- Demonstraits using partial effect
  type LogEffs a = forall es. (Out ApEvent :> es) => Eff es a

  Hook has all the effects of the application but will compile with
  an action that only requires a sublist of these effects
-}
logReturnInt :: IO Int
logReturnInt = log "Returning One" >> pure 1

runSomethingToDoWithTestDepth :: Depth -> IO ()
runSomethingToDoWithTestDepth = logShow

demoOnceAfterHook :: Hook Once After () ()
demoOnceAfterHook =
    AfterHook
        { afterAction = log "After all tests"
        , afterId = "demoOnceAfterHook"
        }

intOnceHook :: Hook Once Before () Int
intOnceHook =
    BeforeHook'
        { depends = demoOnceAfterHook
        , action' = \_void -> logReturnInt
        , beforeId' = "intOnceHook"
        }

intOnceHook2 :: Hook Once Before () Int
intOnceHook2 =
    BeforeHook
        { action = do
            log "2222222 should only be onceeee"
            logReturnInt
        , beforeId = "intOnceHook2"
        }

addOnceIntHook :: Hook Once Before Int Int
addOnceIntHook =
    BeforeHook'
        { depends = intOnceHook
        , action' =
            \i -> do
                log $ "beforeAll' " <> txt i
                pure $ i + 1
        , beforeId' = "addOnceIntHook"
        }

_intThreadHook :: Hook Thread Before () Int
_intThreadHook =
    BeforeHook
        { action = do
            log "deriving meaning of life' "
            pure 42
        , beforeId = "_intThreadHook"
        }

data HookInfo = HookInfo
    { message :: Text
    , value :: Int
    }
    deriving (Show, Generic)

infoThreadHook :: Hook Thread Before Int HookInfo
infoThreadHook =
    BeforeHook'
        { depends = addOnceIntHook
        , action' = \i -> do
            log $ "beforeThread' " <> txt i
            pure $ HookInfo "Hello there" i
        , beforeId' = "infoThreadHook"
        }

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
        , aroundId' = "eachInfoAround"
        }

eachAfter :: Hook Each After Int Int
eachAfter =
    AfterHook'
        { afterDepends = eachInfoAround
        , afterAction' = do
            log "eachAfter"
            pure ()
        , afterId' = "eachAfter"
        }

eachIntBefore :: Hook Each Before Int Int
eachIntBefore =
    BeforeHook'
        { depends = eachInfoAround
        , action' = \hi -> do
            log "eachSetup"
            pure $ hi + 1
        , beforeId' = "eachIntBefore"
        }

-- ############### Test the Lot ###################

config :: TestConfig
config = TestConfig "test" DeepRegression

test :: Fixture ()
test = Full config action parse items

action :: Item -> IO ApState
action itm = do
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

data Item = Item
    { id :: Int
    , title :: Text
    , value :: Int
    , checks :: Checks DState
    }
    deriving (Show, Read)

items :: [Item]
items =
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

test2 :: Fixture HookInfo
test2 = Full' config2 infoThreadHook action2 parse2 items2

action2 :: HookInfo -> Item2 -> IO AS
action2 HookInfo{value = hookVal} itm = do
    logShow itm
    -- when (country == AU) $
    --     log "Aus test"
    -- when (country == NZ) $
    --     log "NZ test"
    -- runSomethingToDoWithTestDepth depth
    -- unless (environment == Prod) $
    --     log "Completing payment with test credit card"
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

items2 :: [Item2]
items2 =
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

test3 :: Fixture Int
test3 =
    Full'
        { depends = eachIntBefore
        , config' = TestConfig "test" DeepRegression
        , action' = \hkInt itm -> do
            log $ txt itm
            pure $ AS (itm.value + 1 + hkInt) $ txt itm.value
        , parse' = \AS{..} -> pure DS{..}
        , items' =
            [ Item2
                { id = 1
                , title = "test the value is one"
                , value = 2
                , checks = chk "test" ((== 1) . (.value))
                }
            ]
        }

-- ############### Test Direct (Record) ###################
test4 :: Fixture Int
test4 =
    Direct'
        { config' = TestConfig "test" DeepRegression
        , depends = eachAfter
        , action' = \_hi itm -> do
            log $ txt itm
            pure $ DS (itm.value + 1) $ txt itm.value
        , items' =
            [ Item2
                { id = 1
                , title = "test the value is one"
                , value = 2
                , checks = chk "test" ((== 1) . (.value))
                }
            , Item2
                { id = 1
                , title = "test the value is one 2 "
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
-- after deriveJson and Item
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

nodes =
    Fixture (NodePath "module" "testName") test4

nodes2 =
    Hook
        { path = NodePath "module" "name"
        , hook = intOnceHook
        , subNodes =
            [ nodes
            , nodes
            ]
        }

--
-- nodes =
--     Hook
--         { path = NodePath "module" "name"
--         , hook = intOnceHook
--         , subNodes = [Fixture (NodePath "module" "testName") test4]
--         }

coreNode = mkNode nodes
coreNode2 = mkNode nodes2

result = runNode coreNode
result2 = runNode coreNode2

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
