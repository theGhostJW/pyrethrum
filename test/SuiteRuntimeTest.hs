module SuiteRuntimeTest where

-- TODO review PyrethrumExtras.Test remove hedgehog in favour of falsify

import Filter (FilterResult (..))
import FullSuiteTestTemplate (Directive (..), Spec (..), SpecGen (..))
import FullSuiteTestTemplate qualified as T
import Internal.SuiteRuntime (ThreadCount (..))
import Internal.SuiteValidation
import PyrethrumExtras (txt)
import PyrethrumExtras.Test (chk, chkEq)
import SuiteRuntimeTestBase hiding (LogResult (..))
import Prelude hiding (All, bug, id)

-- TODO :: chkJust, chkNothing

chkInitFailure :: Maybe Text -> [FilterResult Text] -> IO ()
chkInitFailure expected filterResults =
  expected
    & maybe
      (chk $ isNothing actualFail)
      (\expFail -> chkEq (Just expFail) actualFail)
  where
    actualFail = (.failure) <$> chkSuite filterResults

-- $ > unit_configError_valid_pass

unit_configError_valid_pass :: IO ()
unit_configError_valid_pass =
  chkInitFailure Nothing [MkFilterResult "" Nothing]

-- $ > unit_configError_valid_fail

unit_configError_valid_fail :: IO ()
unit_configError_valid_fail =
  chkInitFailure
    Nothing
    [ MkFilterResult "" $ Just "Failed", -- filtered out
      MkFilterResult "1" Nothing
    ]

-- $ > unit_configError_empty

unit_configError_empty :: IO ()
unit_configError_empty = chkInitFailure (Just "Filtered Test Suite is Empty") []

-- $ > unit_configError_duplicate

unit_configError_duplicate :: IO ()
unit_configError_duplicate =
  chkInitFailure
    (Just "Duplicate Fixture Title")
    [ MkFilterResult "1" $ Just "Failed",
      MkFilterResult "2" $ Just "Failed",
      MkFilterResult "3" $ Just "Failed",
      MkFilterResult "1" Nothing
    ]

-- $ > unit_simple_pass
unit_simple_pass :: IO ()
unit_simple_pass = runTest defaultSeed (ThreadCount 1) [onceAround Pass Pass [fixture [test Pass, test Pass]]]

-- $ > unit_simple_fail

unit_simple_fail :: IO ()
unit_simple_fail = runTest defaultSeed (ThreadCount 1) [onceAround Fail Pass [fixture [test Pass, test Fail]]]

-- >>> unit_nested_pass_fail
unit_nested_pass_fail :: IO ()
unit_nested_pass_fail =
  runTest
    defaultSeed
    (ThreadCount 1)
    [ onceAround
        Pass
        Pass
        [ threadAround Pass Pass [eachAfter Pass [fixture [test Fail, test Pass]]],
          threadAround Pass Pass [eachAfter Fail [fixture [test Pass, test Fail]]],
          threadAround Fail Pass [eachAfter Pass [fixture [test Fail, test Pass]]],
          threadAround Pass Pass [eachBefore Fail [fixture [test Fail, test Pass]]],
          eachAround Fail Pass [fixture [test Fail, test Pass]],
          eachBefore
            Fail
            [ fixture [test Pass, test Pass],
              eachAround
                Pass
                Pass
                [ fixture
                    [ test Pass,
                      test Pass
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
        [ ThreadAround passProb50 passProb100 [eachAfter Pass [fixture [test Fail, test Pass]]],
          ThreadAround passProb100 passProb100 [eachAfter Fail [fixture [test Pass, test Fail]]],
          ThreadAround passProb75 passProb20 [eachAfter Pass [fixture [test Fail, test Pass]]],
          ThreadAround passProb0 passProb100 [eachBefore Fail [fixture [test Fail, test Pass]]],
          EachAround passProb75 passProb75 [fixture [test Fail, test Pass]],
          EachBefore
            passProb100
            [ fixture [test Pass, test Pass],
              EachAround
                passProb100
                passProb75
                [ fixture
                    [ test Pass,
                      test Pass
                    ]
                ]
            ]
        ]
    ]
  where
    passProb pcnt = T.PassProb specGen pcnt 0 100 1000
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
                        [ Spec 0 Pass,
                          Spec 1000 Fail,
                          Spec 100 Fail,
                          Spec 100 Fail,
                          Spec 100 Fail,
                          Spec 100 Fail,
                          Spec 100 Fail
                        ]
                    ]
                ],
              ThreadAround
                (allSpec 100 Pass)
                (allSpec 0 Fail)
                [ ThreadAround
                    (allSpec 300 Pass)
                    (allSpec 300 Pass)
                    [ threadAround Pass Pass [eachAfter Pass [fixture [Spec 3000 Fail, Spec 1000 Pass]]],
                      ThreadAround (allSpec 50 Pass) (allSpec 0 Pass) [eachAfter Fail [fixture [Spec 1000 Pass, Spec 1000 Fail]]],
                      threadAround Fail Pass [eachAfter Pass [fixture [Spec 1000 Fail, Spec 1000 Pass]]],
                      threadAround Pass Pass [EachBefore (allSpec 300 Fail) [fixture [Spec 1000 Fail, Spec 3000 Pass]]],
                      eachAround Fail Pass [fixture [Spec 40 Fail, Spec 10 Pass]],
                      eachBefore
                        Fail
                        [ fixture [Spec 300 Pass, Spec 10 Pass],
                          EachAround
                            (allSpec 50 Pass)
                            (allSpec 0 Pass)
                            [ fixture
                                [ Spec 1000 Pass,
                                  Spec 200 Pass
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
    chkEmptyLog r = chkEq' ("Log should only have start and a end log:\n" <> txt r.log) 2 (length r.log)
    chkLogLength r = chkEq' ("Log length not as expected:\n" <> txt r.log) 12 (length r.log)

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
  selectors being flipped when genrating action (mknAction)
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
              { genStrategy = Preload,
                passPcnt = 95,
                hookPassThroughErrPcnt = 0,
                minDelay = 0,
                maxDelay = 0
              },
          subNodes =
            [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
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
        { eachSpec = T.All Spec {delay = 0, directive = Pass},
          subNodes =
            [ EachAfter
                { eachSpec = T.All Spec {delay = 0, directive = Pass},
                  subNodes =
                    [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
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
        { eachSpec = T.All Spec {delay = 0, directive = Pass},
          subNodes =
            [ EachBefore
                { eachSpec = T.All Spec {delay = 0, directive = Pass},
                  subNodes =
                    [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
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
              { genStrategy = Preload,
                passPcnt = 95,
                hookPassThroughErrPcnt = 0,
                minDelay = 0,
                maxDelay = 0
              },
          subNodes =
            [ Fixture
                { tests =
                    [ Spec {delay = 0, directive = Pass},
                      Spec {delay = 0, directive = Pass}
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
        { eachSetupSpec = T.All $ Spec {delay = 0, directive = Pass},
          eachTeardownSpec = T.All $ Spec {delay = 0, directive = Pass},
          subNodes =
            [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
        }
    ]

-- $ > unit_wrong_result

unit_wrong_result :: IO ()
unit_wrong_result =
  runTest
    defaultSeed
    (ThreadCount 1)
    [ ThreadAfter
        { threadSpec = T.All $ Spec {delay = 0, directive = Pass},
          subNodes =
            [ ThreadBefore
                { threadSpec = T.All $ Spec {delay = 0, directive = Pass},
                  subNodes =
                    [ ThreadAfter
                        { threadSpec =
                            T.PassProb
                              { genStrategy = Preload,
                                passPcnt = 100,
                                hookPassThroughErrPcnt = 0,
                                minDelay = 0,
                                maxDelay = 0
                              },
                          subNodes =
                            [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
                        }
                    ]
                }
            ]
        }
    ]

-- TODO: resolve WSL UNC issues
-- document forgotten password :: sudo chown -R usertename:username ~/pyrethrum/pyrethrum

-- $ > unit_fail_wrong_counts

unit_fail_wrong_counts :: IO ()
unit_fail_wrong_counts =
  do
    putStrLn "unit_fail_wrong_counts"
    replicateM_ 1000 mayFail

mayFail :: IO ()
mayFail =
  runTest
    defaultSeed
    (ThreadCount 5)
    [ Fixture {tests = [Spec {delay = 0, directive = Pass}]},
      OnceBefore
        { spec = Spec {delay = 0, directive = Pass},
          subNodes =
            [ ThreadAfter
                { threadSpec =
                    T.PassProb
                      { genStrategy = Preload,
                        passPcnt = 95,
                        hookPassThroughErrPcnt = 0,
                        minDelay = 0,
                        maxDelay = 0
                      },
                  subNodes =
                    [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
                }
            ]
        }
    ]

-- $ > unit_missing_hooks

unit_missing_hooks :: IO ()
unit_missing_hooks = replicateM_ 1 missingHookFail

missingHookFail :: IO ()
missingHookFail =
  runTest
    defaultSeed
    (ThreadCount 5)
    [ ThreadAround
        { setupThreadSpec = T.All $ Spec {delay = 0, directive = Pass},
          teardownThreadSpec = T.All $ Spec {delay = 0, directive = Pass},
          subNodes =
            [ ThreadAfter
                { threadSpec = T.All $ Spec {delay = 0, directive = Pass},
                  subNodes =
                    [ Fixture
                        { tests =
                            [ Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass}
                            ]
                        }
                    ]
                }
            ]
        }
    ]

-- $ > unit_wrong_failure_path

unit_wrong_failure_path :: IO ()
unit_wrong_failure_path = replicateM_ 1 wrongFailurePath

wrongFailurePath :: IO ()
wrongFailurePath =
  runTest
    defaultSeed
    (ThreadCount 5)
    [ Fixture {tests = [Spec {delay = 0, directive = Pass}]},
      ThreadAfter
        { threadSpec = T.All Spec {delay = 0, directive = Pass},
          subNodes =
            [ ThreadAround
                { setupThreadSpec =
                    T.PassProb
                      { genStrategy = Preload,
                        passPcnt = 95,
                        hookPassThroughErrPcnt = 0,
                        minDelay = 0,
                        maxDelay = 2
                      },
                  teardownThreadSpec = T.All Spec {delay = 0, directive = Pass},
                  subNodes =
                    [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
                }
            ]
        },
      OnceBefore
        { spec = Spec {delay = 0, directive = Pass},
          subNodes =
            [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
        }
    ]

-- $ > unit_once_failure_missing

unit_once_failure_missing :: IO ()
unit_once_failure_missing =
  runTest
    defaultSeed
    (ThreadCount 1)
    [ OnceBefore
        { spec = Spec {delay = 0, directive = Fail},
          subNodes =
            [ ThreadAround
                { setupThreadSpec = T.All Spec {delay = 0, directive = Pass},
                  teardownThreadSpec = T.All Spec {delay = 0, directive = Pass},
                  subNodes =
                    [ ThreadBefore
                        { threadSpec = T.All Spec {delay = 0, directive = Pass},
                          subNodes =
                            [ ThreadAfter
                                { threadSpec = T.All Spec {delay = 0, directive = Pass},
                                  subNodes =
                                    [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
                                }
                            ]
                        }
                    ]
                }
            ]
        }
    ]

-- $ > unit_fixture_passthrough

unit_fixture_passthrough :: IO ()
unit_fixture_passthrough =
  runTest
    defaultSeed
    (ThreadCount 1)
    [ Fixture
        { tests = [Spec {delay = 0, directive = PassThroughFail}]
        }
    ]

-- >>> unit_once_hook_passthrough
unit_once_hook_passthrough :: IO ()
unit_once_hook_passthrough =
  runTest
    defaultSeed
    (ThreadCount 1)
    [ OnceBefore
        { spec = Spec {delay = 0, directive = PassThroughFail},
          subNodes =
            [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
        }
    ]

-- >>> unit_thread_hook_passthrough

-- *** Exception: HUnitFailure (Just (SrcLoc {srcLocPackage = "pyrethrum-extras-0.1.0.0-45a391d86e6b7dfbde63f5b7b8cbfda38e90a6a643d2e35c818247d1adcb93d9", srcLocModule = "PyrethrumExtras.Test.Tasty.HUnit.Extended", srcLocFile = "src/PyrethrumExtras/Test/Tasty/HUnit/Extended.hs", srcLocStartLine = 72, srcLocStartCol = 15, srcLocEndLine = 72, srcLocEndCol = 25})) "Unexpected result for:\n MkEventPath\n  { path = TestPath { id = 0 , title = \"0.0.Test #0\" }\n  , nodeType = Test\n  }\n   expected: All Pass\n  actual: [ Fail ]"

unit_thread_hook_passthrough :: IO ()
unit_thread_hook_passthrough =
  runTest
    defaultSeed
    (ThreadCount 1)
    [ ThreadBefore
        { threadSpec = T.All $ Spec {delay = 0, directive = PassThroughFail},
          subNodes =
            [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
        }
    ]

-- >>> unit_basic_each_before
unit_basic_each_before :: IO ()
unit_basic_each_before =
  runTest
    defaultSeed
    (ThreadCount 1)
    [ EachBefore
        { eachSpec = T.All {spec = Spec {delay = 0, directive = Pass}},
          subNodes =
            [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
        }
    ]

-- >>> unit_another_broken_test
unit_another_broken_test :: IO ()
unit_another_broken_test =
  runTest'
    NoLog
    defaultSeed
    (ThreadCount 1)
    [ EachAfter
        { eachSpec = T.All {spec = Spec {delay = 0, directive = Pass}},
          subNodes =
            [ EachAfter
                { eachSpec =
                    T.PassProb
                      { genStrategy = Preload,
                        passPcnt = 90,
                        hookPassThroughErrPcnt = 2,
                        minDelay = 0,
                        maxDelay = 0
                      },
                  subNodes =
                    [ Fixture
                        { tests =
                            [ Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass},
                              Spec {delay = 0, directive = Pass}
                            ]
                        },
                      Fixture {tests = [Spec {delay = 0, directive = Pass}]}
                    ]
                }
            ]
        }
    ]

-- $ > unit_and_another_broken_test

unit_and_another_broken_test :: IO ()
unit_and_another_broken_test =
  runTest'
    NoLog
    defaultSeed
    (ThreadCount 1)
    [ OnceBefore
        { spec = Spec {delay = 0, directive = Pass},
          subNodes =
            [ OnceAfter
                { spec = Spec {delay = 0, directive = Pass},
                  subNodes =
                    [ ThreadBefore
                        { threadSpec = T.All {spec = Spec {delay = 0, directive = Pass}},
                          subNodes =
                            [ ThreadAround
                                { setupThreadSpec =
                                    T.All {spec = Spec {delay = 0, directive = Pass}},
                                  teardownThreadSpec =
                                    T.All {spec = Spec {delay = 0, directive = Pass}},
                                  subNodes =
                                    [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
                                }
                            ]
                        }
                    ]
                }
            ]
        }
    ]

-- >>> unit_and_one_more_broken_test
unit_and_one_more_broken_test :: IO ()
unit_and_one_more_broken_test =
  runTest'
    Log
    defaultSeed
    (ThreadCount 1)
    [ OnceAround
        { setupSpec = Spec {delay = 0, directive = Pass},
          teardownSpec = Spec {delay = 0, directive = Pass},
          subNodes =
            [ EachAfter
                { eachSpec = T.All {spec = Spec {delay = 0, directive = Pass}},
                  subNodes =
                    [ EachAround
                        { eachSetupSpec =
                            T.All {spec = Spec {delay = 0, directive = PassThroughFail}},
                          eachTeardownSpec =
                            T.All {spec = Spec {delay = 0, directive = Pass}},
                          subNodes =
                            [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
                        }
                    ]
                }
            ]
        }
    ]

-- >>> unit_and_here_is_another_broken_test
unit_and_here_is_another_broken_test :: IO ()
unit_and_here_is_another_broken_test =
  runTest'
    Log
    defaultSeed
    (ThreadCount 1)
    [ ThreadAround
        { setupThreadSpec =
            T.All {spec = Spec {delay = 0, directive = Pass}},
          teardownThreadSpec =
            T.All {spec = Spec {delay = 0, directive = Pass}},
          subNodes =
            [ ThreadAfter
                { threadSpec = T.All {spec = Spec {delay = 0, directive = Pass}},
                  subNodes =
                    [ EachAround
                        { eachSetupSpec =
                            T.All {spec = Spec {delay = 0, directive = PassThroughFail}},
                          eachTeardownSpec =
                            T.All {spec = Spec {delay = 0, directive = Pass}},
                          subNodes =
                            [ EachAfter
                                { eachSpec = T.All {spec = Spec {delay = 0, directive = Pass}},
                                  subNodes =
                                    [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
                                }
                            ]
                        }
                    ]
                }
            ]
        }
    ]

-- >>> unit_and_here_is_yet_another_broken_test
unit_and_here_is_yet_another_broken_test :: IO ()
unit_and_here_is_yet_another_broken_test =
  replicateM_ 100 $
    runTest'
      LogFails
      defaultSeed
      (ThreadCount 10)
      [ Fixture {tests = [Spec {delay = 0, directive = Pass}]},
        Fixture {tests = [Spec {delay = 0, directive = Pass}]},
        OnceAfter
          { spec = Spec {delay = 0, directive = Pass},
            subNodes =
              [ OnceBefore
                  { spec = Spec {delay = 0, directive = Fail},
                    subNodes =
                      [ OnceBefore
                          { spec = Spec {delay = 0, directive = Pass},
                            subNodes =
                              [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
                          }
                      ]
                  }
              ]
          },
        ThreadBefore
          { threadSpec =
              T.PassProb
                { genStrategy = Preload,
                  passPcnt = 25,
                  -- , passPcnt = 90
                  hookPassThroughErrPcnt = 50,
                  -- , hookPassThroughErrPcnt = 2
                  minDelay = 0,
                  maxDelay = 0
                },
            subNodes =
              [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
          }
      ]

-- $ > unit_leaf_hook_failure

unit_leaf_hook_failure :: IO ()
unit_leaf_hook_failure =
  replicateM_ 1000 $
    runTest'
      LogFailsAndStartTest
      defaultSeed
      (ThreadCount 5)
      [ Fixture {tests = [Spec {delay = 0, directive = Fail}]},
        OnceBefore
          { spec = Spec {delay = 0, directive = Pass},
            subNodes =
              [ OnceAround
                  { setupSpec = Spec {delay = 0, directive = PassThroughFail},
                    teardownSpec = Spec {delay = 0, directive = Pass},
                    subNodes =
                      [ EachAfter
                          { eachSpec = T.All {spec = Spec {delay = 237, directive = Pass}},
                            subNodes =
                              [ Fixture
                                  { tests =
                                      [ Spec {delay = 344, directive = Pass},
                                        Spec {delay = 36, directive = Pass},
                                        Spec {delay = 2, directive = Pass}
                                      ]
                                  }
                              ]
                          }
                      ]
                  }
              ]
          }
      ]

-- $ > unit_failed_again

unit_failed_again :: IO ()
unit_failed_again =
  replicateM_ 1000 $
    runTest'
      LogFailsAndStartTest
      defaultSeed
      (ThreadCount 5)
      [ OnceAround
          { setupSpec = Spec {delay = 0, directive = Fail},
            teardownSpec = Spec {delay = 0, directive = Pass},
            subNodes =
              [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
          },
        OnceAround
          { setupSpec = Spec {delay = 0, directive = Fail},
            teardownSpec = Spec {delay = 0, directive = Pass},
            subNodes =
              [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
          }
      ]

-- $ > unit_bypass_failed

unit_bypass_failed :: IO ()
unit_bypass_failed =
  replicateM_ 1000 $
    runTest'
      LogFailsAndStartTest
      defaultSeed
      (ThreadCount 5)
      [ EachBefore
          { eachSpec = T.All {spec = Spec {delay = 0, directive = Pass}},
            subNodes =
              [ Fixture {tests = [Spec {delay = 0, directive = Pass}]},
                EachBefore
                  { eachSpec =
                      T.PassProb
                        { genStrategy = Preload,
                          passPcnt = 90,
                          hookPassThroughErrPcnt = 2,
                          minDelay = 0,
                          maxDelay = 0
                        },
                    subNodes =
                      [ Fixture
                          { tests =
                              [ Spec {delay = 0, directive = Pass},
                                Spec {delay = 0, directive = Pass},
                                Spec {delay = 0, directive = Pass},
                                Spec {delay = 0, directive = Pass},
                                Spec {delay = 0, directive = Pass},
                                Spec {delay = 0, directive = Pass},
                                Spec {delay = 0, directive = Pass},
                                Spec {delay = 0, directive = Pass},
                                Spec {delay = 0, directive = Pass},
                                Spec {delay = 0, directive = Pass}
                              ]
                          }
                      ]
                  }
              ]
          }
      ]

-- $ > unit_passthrough_fail

unit_passthrough_fail :: IO ()
unit_passthrough_fail =
  runTest'
    LogFailsAndStartTest
    defaultSeed
    (ThreadCount 1)
    [ OnceBefore
        { spec = Spec {delay = 0, directive = Pass},
          subNodes =
            [ OnceAround
                { setupSpec = Spec {delay = 0, directive = PassThroughFail},
                  teardownSpec = Spec {delay = 0, directive = Pass},
                  subNodes =
                    [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
                }
            ]
        }
    ]


-- $ > unit_passthrough_fail2
unit_passthrough_fail2 :: IO ()
unit_passthrough_fail2 =
  runTest'
    LogFailsAndStartTest
    defaultSeed
    (ThreadCount 1)
    [ OnceAround
        { setupSpec = Spec {delay = 0, directive = PassThroughFail},
          teardownSpec = Spec {delay = 0, directive = Pass},
          subNodes =
            [ EachBefore
                { eachSpec = T.All {spec = Spec {delay = 0, directive = Pass}},
                  subNodes =
                    [Fixture {tests = [Spec {delay = 0, directive = Pass}]}]
                }
            ]
        }
    ]


{-
TODO:
- rethink failure propagation checks :: DONE
- get unit working :: DONE
- rerun prop based tests - runtime - 1000 x 
- rerun prop based tests - runtime - 1000 x - high pass through error rate
- fix once hook pass through error
- rerun prop based test - preload - 1000 x DONE
- rerun prop based tests - runtime - 10000 x DONE
- rerun prop based tests - runtime - 10000 x - DONE - high pass through error rate
- rerun prop based test - preload - 10000 x
- rerun prop based test - preload - 10000 x - high pass through error rate
- delete legacy propagation check - clean up unit tests
- merge
-}