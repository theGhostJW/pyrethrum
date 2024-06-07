module SuiteRuntimeTest where

import FullSuiteTestTemplate (Result (..), Spec (..), SpecGen (..))
import FullSuiteTestTemplate qualified as T
import Internal.SuiteRuntime (ThreadCount (..))

import List.Extra as LE hiding (list)

-- TODO review PyrethrumExtras.Test remove hedgehog in favour of falsify

import PyrethrumExtras (uu)
import SuiteRuntimeTestBase
import Prelude hiding (All, bug, id)

-- $ > unit_simple_pass
unit_simple_pass :: IO ()
unit_simple_pass = print "start" >> runTest defaultSeed (ThreadCount 1) [onceAround Pass Pass [fixture [test Pass, test Fail]]]

-- $ > unit_simple_fail
unit_simple_fail :: IO ()
unit_simple_fail = runTest defaultSeed (ThreadCount 1) [onceAround Fail Pass [fixture [test Pass, test Fail]]]

-- $ > unit_nested_pass_fail
unit_nested_pass_fail :: IO ()
unit_nested_pass_fail =
    runTest
        defaultSeed
        (ThreadCount 1)
        [ onceAround
            Pass
            Pass
            [ threadAround Pass Pass [eachAfter Pass [fixture [test Fail, test Pass]]]
            , threadAround Pass Pass [eachAfter Fail [fixture [test Pass, test Fail]]]
            , threadAround Fail Pass [eachAfter Pass [fixture [test Fail, test Pass]]]
            , threadAround Pass Pass [eachBefore Fail [fixture [test Fail, test Pass]]]
            , eachAround Fail Pass [fixture [test Fail, test Pass]]
            , eachBefore
                Fail
                [ fixture [test Pass, test Pass]
                , eachAround
                    Pass
                    Pass
                    [ fixture
                        [ test Pass
                        , test Pass
                        ]
                    ]
                ]
            ]
        ]

passProbSuite :: SpecGen -> IO ()
passProbSuite specGen =
    runTest
        defaultSeed
        (ThreadCount 1)
        [ onceAround
            Pass
            Pass
            [ ThreadAround passProb50 passProb100 [eachAfter Pass [fixture [test Fail, test Pass]]]
            , ThreadAround passProb100 passProb100 [eachAfter Fail [fixture [test Pass, test Fail]]]
            , ThreadAround passProb75 passProb20 [eachAfter Pass [fixture [test Fail, test Pass]]]
            , ThreadAround passProb0 passProb100 [eachBefore Fail [fixture [test Fail, test Pass]]]
            , EachAround passProb75 passProb75 [fixture [test Fail, test Pass]]
            , EachBefore
                passProb100
                [ fixture [test Pass, test Pass]
                , EachAround
                    passProb100
                    passProb75
                    [ fixture
                        [ test Pass
                        , test Pass
                        ]
                    ]
                ]
            ]
        ]
  where
    passProb pcnt = T.PassProb specGen pcnt 100 1000
    passProb0 = passProb 0
    passProb20 = passProb 20
    passProb50 = passProb 50
    passProb75 = passProb 75
    passProb100 = passProb 100

-- $ > unit_nested_threaded_chk_thread_count
unit_nested_threaded_chk_thread_count :: IO ()
unit_nested_threaded_chk_thread_count =
    do
        let threadLimit = ThreadCount 10
            logging' = NoLog
        r <-
            execute
                logging'
                defaultSeed
                threadLimit
                [ OnceAround
                    (Spec 1000 Pass)
                    (Spec 0 Pass)
                    [ ThreadAround
                        (allSpec 0 Pass)
                        (allSpec 0 Pass)
                        [ EachAfter
                            (allSpec 50 Pass)
                            [ fixture
                                [ Spec 0 Pass
                                , Spec 1000 Fail
                                , Spec 100 Fail
                                , Spec 100 Fail
                                , Spec 100 Fail
                                , Spec 100 Fail
                                , Spec 100 Fail
                                ]
                            ]
                        ]
                    , ThreadAround
                        (allSpec 100 Pass)
                        (allSpec 0 Fail)
                        [ ThreadAround
                            (allSpec 300 Pass)
                            (allSpec 300 Pass)
                            [ threadAround Pass Pass [eachAfter Pass [fixture [Spec 3000 Fail, Spec 1000 Pass]]]
                            , ThreadAround (allSpec 50 Pass) (allSpec 0 Pass) [eachAfter Fail [fixture [Spec 1000 Pass, Spec 1000 Fail]]]
                            , threadAround Fail Pass [eachAfter Pass [fixture [Spec 1000 Fail, Spec 1000 Pass]]]
                            , threadAround Pass Pass [EachBefore (allSpec 300 Fail) [fixture [Spec 1000 Fail, Spec 3000 Pass]]]
                            , eachAround Fail Pass [fixture [Spec 40 Fail, Spec 10 Pass]]
                            , eachBefore
                                Fail
                                [ fixture [Spec 300 Pass, Spec 10 Pass]
                                , EachAround
                                    (allSpec 50 Pass)
                                    (allSpec 0 Pass)
                                    [ fixture
                                        [ Spec 1000 Pass
                                        , Spec 200 Pass
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
        chkProperties defaultSeed threadLimit r.expandedTemplate r.log
        chkThreadCount threadLimit r.log

{- each and once hooks will always run but thread hooks may be empty
   due to subitems being stolen by another thread. We need to ensure
   that empty thread TestTrees are not executed
-}

{-
TODO revist this test after any concrete interpretor is implemented
    that affects laziness ie. ensure empty subnodes are not run if we
    force strictness in hooks
-}

-- $ > unit_empty_thread_hooks
unit_empty_thread_hooks :: IO ()
unit_empty_thread_hooks =
    do
        exe [threadAround Pass Pass []] >>= chkEmptyLog
        exe [threadBefore Pass []] >>= chkEmptyLog
        exe [threadAfter Pass []] >>= chkEmptyLog
        exe [threadAround Pass Pass [threadBefore Pass [threadAfter Pass []]]] >>= chkEmptyLog
        exe [threadAround Pass Pass [threadBefore Pass [threadAfter Pass [fixture [test Pass]]]]] >>= chkLogLength
  where
    exe = execute NoLog defaultSeed (ThreadCount 1)
    chkEmptyLog r = chkEq' ("Log should only have start and end log:\n" <> (ptxt r.log)) 2 (length r.log)
    chkLogLength r = chkEq' ("Log length not as expected:\n" <> (ptxt r.log)) 12 (length r.log)

{-
  todo:

  find out about: +optimise-heavily -f +enable-cluster-counting
  and other compile options

  1. Getting the release candidate

       $ cabal get
https://hackage.haskell.org/package/Agda-2.6.4.3/Agda-2.6.4.3.tar.gz
       $ cd Agda-2.6.4.3

2. a. Using cabal-install

       $ cabal install -f +optimise-heavily -f +enable-cluster-counting
  -}

-- $ > unit_pass_prob_pregen
unit_pass_prob_pregen :: IO ()
unit_pass_prob_pregen = passProbSuite Preload

-- $ > unit_pass_prob_no_pregen
unit_pass_prob_no_pregen :: IO ()
unit_pass_prob_no_pregen = passProbSuite Runtime

{-
  property test revealed incorrect template transformation due to
  selectors being flipped when genrating action (mkManyAction)
  intermittent - hence replicate n

  Commit:        ef509617ef3bf4762e2a3215d192cf3ebab873ca
  Author:        theGhostJW <theghostjw@gmail.com>
  AuthorDate:    Thu May 2 19:23:45 2024
  Commit:        theGhostJW <theghostjw@gmail.com>
  CommitDate:    Thu May 2 19:23:45 2024
  Url:           https://github.com/theGhostJW/pyrethrum/commit/ef50961
-}

-- $ > unit_prop_test_fail_template_wrong_result
unit_prop_test_fail_template_wrong_result :: IO ()
unit_prop_test_fail_template_wrong_result = replicateM_ 10 propFailTemplateGenWrongEachHookResult

propFailTemplateGenWrongEachHookResult :: IO ()
propFailTemplateGenWrongEachHookResult =
    runTest
        defaultSeed
        (ThreadCount 1)
        [ EachBefore
            { eachSpec =
                T.PassProb
                    { genStrategy = Preload
                    , passPcnt = 95
                    , minDelay = 0
                    , maxDelay = 0
                    }
            , subNodes =
                [Fixture{tests = [Spec{delay = 0, result = Pass}]}]
            }
        ]

{--
  property test revealed nested AfterEach hooks firing out of order
-}
--

-- $ > unit_prop_fail_each_after_out_of_order
unit_prop_fail_each_after_out_of_order :: IO ()
unit_prop_fail_each_after_out_of_order =
    runTest
        defaultSeed
        (ThreadCount 1)
        [ EachAfter
            { eachSpec = T.All Spec{delay = 0, result = Pass}
            , subNodes =
                [ EachAfter
                    { eachSpec = T.All Spec{delay = 0, result = Pass}
                    , subNodes =
                        [Fixture{tests = [Spec{delay = 0, result = Pass}]}]
                    }
                ]
            }
        ]

-- $ > unit_prop_fail_each_after_out_of_order1
unit_prop_fail_each_after_out_of_order1 :: IO ()
unit_prop_fail_each_after_out_of_order1 =
    runTest
        defaultSeed
        (ThreadCount 1)
        [ EachAfter
            { eachSpec = T.All Spec{delay = 0, result = Pass}
            , subNodes =
                [ EachBefore
                    { eachSpec = T.All Spec{delay = 0, result = Pass}
                    , subNodes =
                        [Fixture{tests = [Spec{delay = 0, result = Pass}]}]
                    }
                ]
            }
        ]

-- $ > unit_prop_fail_each_after
unit_prop_fail_each_after :: IO ()
unit_prop_fail_each_after =
    runTest
        defaultSeed
        (ThreadCount 1)
        [ EachAfter
            { eachSpec =
                T.PassProb
                    { genStrategy = Preload
                    , passPcnt = 95
                    , minDelay = 0
                    , maxDelay = 0
                    }
            , subNodes =
                [ Fixture
                    { tests =
                        [ Spec{delay = 0, result = Pass}
                        , Spec{delay = 0, result = Pass}
                        ]
                    }
                ]
            }
        ]

-- $ > unit_missing_setup
unit_missing_setup :: IO ()
unit_missing_setup =
    runTest
        defaultSeed
        (ThreadCount 1)
        [ EachAround
            { eachSetupSpec = T.All $ Spec{delay = 0, result = Pass}
            , eachTeardownSpec = T.All $ Spec{delay = 0, result = Pass}
            , subNodes =
                [Fixture{tests = [Spec{delay = 0, result = Pass}]}]
            }
        ]

-- $ > unit_wrong_result
unit_wrong_result :: IO ()
unit_wrong_result =
    runTest
        defaultSeed
        (ThreadCount 1)
        [ ThreadAfter
            { threadSpec = T.All $ Spec{delay = 0, result = Pass}
            , subNodes =
                [ ThreadBefore
                    { threadSpec = T.All $ Spec{delay = 0, result = Pass}
                    , subNodes =
                        [ ThreadAfter
                            { threadSpec =
                                T.PassProb
                                    { genStrategy = Preload
                                    , passPcnt = 100
                                    , minDelay = 0
                                    , maxDelay = 0
                                    }
                            , subNodes =
                                [Fixture{tests = [Spec{delay = 0, result = Pass}]}]
                            }
                        ]
                    }
                ]
            }
        ]



-- $> unit_failed
unit_failed :: IO ()
unit_failed = replicateM_ 10 mightFail

mightFail =
    runTest'
        Log
        defaultSeed
        (ThreadCount 5)
        [ OnceAround { setupSpec = Spec { delay = 0 , result = Fail }
                       , teardownSpec = Spec { delay = 0 , result = Pass }
                       , subNodes = [ Fixture { tests = [ Spec { delay = 0 , result = Pass } ] } ]
        }
    ]

{-
 $> unit_failed
#### Template ####
[ OnceAround
    { path = NodePath { module' = "0" , path = "OnceAround" }
    , setupSpec = Spec { delay = 0 , result = Fail }
    , teardownSpec = Spec { delay = 0 , result = Pass }
    , subNodes =
        [ Fixture
            { path = NodePath { module' = "0.0" , path = "Test" }
            , tests =
                [ TestItem
                    { id = 0
                    , title = "0.0 Test"
                    , spec = Spec { delay = 0 , result = Pass }
                    }
                ]
            }
        ]
    }
]
=========
#### (Indent, Node Path) After Prepare ####
[ [ ( 0 , NodePath { module' = "0" , path = "OnceAround" } )
  , ( 1 , NodePath { module' = "0.0" , path = "Test" } )
  ]
]
=========
#### Log ####
StartExecution { idx = 0 , threadId = 3663 }
Start
  { idx = 0
  , threadId = 3664
  , suiteEvent = Hook Once Setup
  , loc =
      ExePath
        { un = [ NodePath { module' = "0" , path = "OnceAround" } ] }
  }
Failure
  { idx = 1
  , threadId = 3664
  , suiteEvent = Hook Once Setup
  , loc =
      ExePath
        { un = [ NodePath { module' = "0" , path = "OnceAround" } ] }
  , exception =
      PException
        { displayText =
            [ "\"NodePath {module' = \\\"0\\\", path = \\\"OnceAround\\\"} failed\""
            , "\"CallStack (from HasCallStack):\""
            , "\"  error, called at src/Relude/Debug.hs:296:11 in relude-1.2.1.0-d785bc774ce2cb944aefabcc579bcc6aa958f3b3aa563694f6184f924047743c:Relude.Debug\""
            , "\"  error, called at test/SuiteRuntimeTestBase.hs:1019:7 in pyrethrum-0.1.0.0-inplace:SuiteRuntimeTestBase\""
            ]
        }
  }
End
  { idx = 2
  , threadId = 3664
  , suiteEvent = Hook Once Setup
  , loc =
      ExePath
        { un = [ NodePath { module' = "0" , path = "OnceAround" } ] }
  }
ParentFailure
  { idx = 3
  , threadId = 3664
  , loc =
      ExePath
        { un =
            [ TestPath { id = 0 , title = "0.0 Test" }
            , NodePath { module' = "0.0" , path = "Test" }
            , NodePath { module' = "0" , path = "OnceAround" }
            ]
        }
  , suiteEvent = Test
  , failLoc =
      ExePath
        { un = [ NodePath { module' = "0" , path = "OnceAround" } ] }
  , failSuiteEvent = Hook Once Setup
  }
ParentFailure
  { idx = 0
  , threadId = 3665
  , loc =
      ExePath
        { un = [ NodePath { module' = "0" , path = "OnceAround" } ] }
  , suiteEvent = Hook Once Teardown
  , failLoc =
      ExePath
        { un = [ NodePath { module' = "0" , path = "OnceAround" } ] }
  , failSuiteEvent = Hook Once Setup
  }
EndExecution { idx = 1 , threadId = 3663 }
*** Exception: user error (
subsequent parent event for:
SuiteEventPath
  { path = TestPath { id = 0 , title = "0.0 Test" }
  , suiteEvent = Test
  }
equality check failed:
Expected:
  Just
  SuiteEventPath
    { path = NodePath { module' = "0" , path = "OnceAround" }
    , suiteEvent = Hook Once Teardown
    }
Does not Equal:
  Nothing
)   
    
    
    
    
-}