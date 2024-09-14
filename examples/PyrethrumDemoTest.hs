module PyrethrumDemoTest where

import Check (Checks, chk)
import Core (After, Around, Before, Each, Once, ParseException, Thread)
import Core qualified as C
import DSL.Internal.NodeEvent (NodeEvent (..), Path (..), ULog (Log))
import DSL.Out (out)
import Effectful (Eff)
import PyrethrumBase (
    Action,
    DataSource (..),
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
import PyrethrumConfigTypes (Country (..), Environment (Prod, TST))
import PyrethrumExtras (txt)

{-
Note:: tried alternative with individual hook types but the results
were starting to look more complex than the original so abandonned.
-}

log :: (C.Logger m) => Text -> m ()
log = C.tLog . show . User . Log

logShow :: (C.Logger m, Show a) => a -> m ()
logShow = C.tLog . show . User . Log . txt

{- Demonstraits using partial effect
  type LogEffs a = forall es. (Out NodeEvent :> es) => Eff es a

  Hook has all the effects of the application but will compile with
  an action that only requires a sublist of these effects
-}
logReturnInt :: RunConfig -> IO Int
logReturnInt _ = log "Returning One" >> pure 1

runSomethingToDoWithTestDepth :: Depth -> Action ()
runSomethingToDoWithTestDepth = logShow

demoOnceAfterHook :: Hook Once After () ()
demoOnceAfterHook =
    AfterHook
        { afterAction = const $ log "After all tests"
        }

intOnceHook :: Hook Once Before () Int
intOnceHook =
    BeforeHook'
        { depends = demoOnceAfterHook
        , action' = \rc _void -> logReturnInt rc
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
    AfterHook'
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

-- ############### Test the Lot ###################

config :: TestConfig
config = TestConfig "test the lot" DeepRegression

test1 :: Fixture ()
test1 = Full config action parse items

action :: RunConfig -> Item -> Action ApState
action _expectedrc itm = do
    log $ txt "test the lot: " <> txt itm
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

items :: RunConfig -> DataSource Item
items =
    const $
        C.ItemList
            [ Item
                { id = 1
                , title = "test the value is one"
                , value = 2
                , checks = chk "test" ((== 1) . (.value))
                }
            ]

-- ############### Test the Lot Child ###################

config2 :: TestConfig
config2 = TestConfig "test Test the Lot Child" DeepRegression

test2 :: Fixture HookInfo
test2 = Full' config2 infoThreadHook action2 parse2 items2

action2 :: RunConfig -> HookInfo -> Item2 -> Action AS
action2 RunConfig{country, depth, environment} HookInfo{value = hookVal} itm = do
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

items2 :: RunConfig -> DataSource Item2
items2 rc =
    ItemList $
        filter
            (\i -> rc.depth == Regression || i.id < 10)
            [ Item2
                { id = 1
                , title = "test the lot child"
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
        , config' = TestConfig "Test the Lot (Record)" DeepRegression
        , action' = \_rc hkInt itm -> do
            log $ txt itm
            pure $ AS (itm.value + 1 + hkInt) $ txt itm.value
        , parse' = \AS{..} -> pure DS{..}
        , items' =
            const
                . ItemList
                $ [ Item2
                        { id = 1
                        , title = "test the value is one - Test the Lot (Record)"
                        , value = 2
                        , checks = chk "test" ((== 1) . (.value))
                        }
                  ]
        }

-- ############### Test the Lot (Record) ###################

test6 :: Fixture Int
test6 =
    Full'
        { depends = eachIntBefore
        , config' = TestConfig "6 Test the Lot (Record)" DeepRegression
        , action' = \_rc hkInt itm -> do
            log $ txt itm
            pure $ AS (itm.value + 1 + hkInt) $ txt itm.value
        , parse' = \AS{..} -> pure DS{..}
        , items' =
            const
                . ItemList
                $ [ Item2
                        { id = 1000
                        , title = "test 6 the value is one - Test the Lot (Record)"
                        , value = 2
                        , checks = chk "test" ((== 1) . (.value))
                        }
                  ]
        }

-- ############### Test the Lot (Record) ###################

test5 :: Fixture Int
test5 =
    Full'
        { depends = eachIntBefore
        , config' = TestConfig "Test the Lot (Record) 5" DeepRegression
        , action' = \_rc hkInt itm -> do
            log $ txt itm
            pure $ AS (itm.value + 1 + hkInt) $ txt itm.value
        , parse' = \AS{..} -> pure DS{..}
        , items' =
            const
                . ItemList
                $ [ Item2
                        { id = 2
                        , title = "Test the Lot (Record) 5"
                        , value = 2
                        , checks = chk "test" ((== 1) . (.value))
                        }
                  ]
        }

-- ############### Test Direct (Record) ###################
test4 :: Fixture Int
test4 =
    Direct'
        { config' = TestConfig "test4 Test Direct (Record)" DeepRegression
        , depends = eachAfter
        , action' = \_rc _hi itm -> do
            log $ txt itm
            pure $ DS (itm.value + 1) $ txt itm.value
        , items' =
            const
                . ItemList
                $ [ Item2
                        { id = 1
                        , title = "I Test Direct (Record) :: test4: test the value is one"
                        , value = 2
                        , checks = chk "test" ((== 1) . (.value))
                        },
                    Item2
                        { id = 2
                        , title = "II Test Direct (Record) :: test4: test the value is one"
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

node :: Node ()
node =
    Hook
        { path = NodePath "module" "name"
        , hook = demoOnceAfterHook
        , subNodes =
            [ Hook
                { path = NodePath "module" "name"
                , hook = intOnceHook
                , subNodes =
                    [ Fixture (NodePath "module" "testName") test3
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
                                            [ Fixture (NodePath "module" "testName") test4
                                            , Hook
                                                { path = NodePath "module" "name"
                                                , hook = eachAfter
                                                , subNodes =
                                                    [ Fixture (NodePath "module" "testName") test5
                                                    , Fixture (NodePath "module" "testName") test6
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

runConfig :: RunConfig
runConfig =
    RunConfig
        { title = "rc"
        , environment = TST
        , maxThreads = 10
        , country = AU
        , depth = DeepRegression
        }

coreNode :: C.Node Action RunConfig TestConfig ()
coreNode = mkNode node


{-
?? Questions ??
- plug in existing tests
- Once hooks
- test for empty thread hooks
- would noop hooks result in logging empty hooks?
- logging abandonned (skipped) tests and hooks
- how switch interpreters 

- how to control no of threads it looks like this could potentially run as many threads as there are items ?
    - https://hackage.haskell.org/package/unliftio-0.2.25.0/docs/UnliftIO-Internals-Async.html#v:pooledMapConcurrentlyN ?
    - note difference with my code below
    - how are tests distributed between threads (depth or breath first)
      - when a thread run to the end of one branch does it expire or recycle?
      - the behaviour of main should be (limited or no unit test)
        - t1 traverses to end of base branch 1
        - t2 traverses to end of base branch 2
        - when a tread 1 finishes it drops back a node and executes the next highest branch
        - when thread 1 finishes branch 1 it cycles back and starts down branch 2 prioritising unstarted sub-branches
        - finally, when all other branches are complete, thread 1 will eventually start running the same fixture as thread 2 if thread 2 is still on that fixture 
        - this behaviour is achieved via nested childQueues (not sure if the same can be achieved with PO forConcurrently_)
-- note difference master branch code
executeNodes :: L.LoggerSource (L.Event L.ExePath AE.NodeEvent) -> ChildQ (ExeTree ()) -> ThreadCount -> IO ()
executeNodes L.MkLoggerSource {rootLogger, newLogger} nodes tc =
  do
    finally
      ( rootLogger L.StartExecution
          >> forConcurrently_
            thrdTokens   <<<<----- THIS
            ( const do
                logger <- newLogger
                runChildQ Concurrent (runNode logger $ OnceIn ()) canRunXTree nodes  <<<<----- THIS
            )
      )
      (rootLogger L.EndExecution)
  where
    thrdTokens = replicate tc.maxThreads True
-}

result :: Action ()
result = C.runNode runConfig (pure ()) (const $ pure ()) coreNode
-- >>> result

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
