module SuiteRuntimeTest where

import FullSuiteTestTemplate (Result (..), Spec (..), SpecGen (..))
import FullSuiteTestTemplate qualified as T
import Internal.SuiteRuntime (ThreadCount (..))


import List.Extra as LE hiding (list)

-- TODO review PyrethrumExtras.Test remove hedgehog in favour of falsify

import Prelude hiding (All, bug, id)
import SuiteRuntimeTestBase

-- $ > unit_simple_pass
unit_simple_pass :: IO ()
unit_simple_pass = runTest defaultSeed (ThreadCount 1) [onceAround Pass Pass [fixture [test Pass, test Fail]]]

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

-- $ > unit_empty_thread_around
unit_empty_thread_around :: IO ()
unit_empty_thread_around =
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
