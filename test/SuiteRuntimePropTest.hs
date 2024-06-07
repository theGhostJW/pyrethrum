{-# OPTIONS_GHC -fno-cse #-}
-- https://hackage.haskell.org/package/base-4.19.1.0/docs/System-IO-Unsafe.html
{-# OPTIONS_GHC -fno-full-laziness #-}

module SuiteRuntimePropTest where

import FullSuiteTestTemplate (Result (..), Spec (..), SpecGen (..))
import FullSuiteTestTemplate qualified as T

import PyrethrumExtras
import SuiteRuntimeTestBase

-- TODO review PyrethrumExtras.Test remove hedgehog in favour of falsify

import System.Random.Stateful qualified as RS
import Test.Falsify.Generator as G (Gen, frequency, inRange, list)
import Test.Falsify.Predicate qualified as FP
import Text.Show.Pretty (pPrint, ppShow)
import Prelude hiding (All, bug, id)

import BasePrelude (unsafePerformIO)
import Internal.SuiteRuntime (ThreadCount (..))
import Internal.ThreadEvent (Hz (..))
import Test.Falsify.Range (between, skewedBy)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify (
  ExpectFailure (DontExpectFailure),
  TestOptions (..),
  Verbose (Verbose),
  assert,
  collect,
  gen,
  genWith,
  testPropertyWith,
 )
import UnliftIO (tryAny)


{-
 - each fail
 - thread fail
-}

-- $ > genPlay
genPlay :: IO ()
genPlay = do
  i <- RS.uniformM RS.globalStdGen :: IO Int
  pPrint i
  rg <- RS.randomRIO (1, 100)
  pPrint rg

--  todo :: simple random api / effect
-- generate [Template]
-- generate Template
-- generate Test
-- generate Spec

-- HERE !!!!!
-- gen delay
-- gen result

{- As property based tests have been implemented after "unit tests" and there has already been
   arbitary behavior implemented with respect to test results and test durations, these properties
   will not be subject to to generation or shrinking by the property based testing library.
   It would be too much work replace the existing behavior with the property based testing properties
   for these attributes.
-}

{- simplified
   Reddit post:
   https://www.reddit.com/r/haskell/comments/1bsnpk2/comment/l0iw3bf/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
genNode :: GenParams -> Gen Template
genNode gl@GenParams{
  maxDepth,
  maxDelay,
  maxBranches,
  maxTests,
  passPcnt} =
  frequency
    [ (fixtureWeight, genFixture)
    , (hkWeight, genOnceBefore)
    ]
 where
  hkWeight = 20
  fixtureWeight = 100 - hkWeight * 2
  nxtLimits = gl{maxDepth = maxDepth - 1}
  genSubnodes = list (between (1, maxBranches))
  genOnceSubnodes = genSubnodes $ genNode (nxtLimits{minHz = Once})
  genSpec' = genSpec maxDelay passPcnt
  genOnceBefore = OnceBefore <$> genSpec' <*> genOnceSubnodes
  genFixture = Fixture <$> (list (between (1, maxTests)) $ genSpec')
-}

defaultTestOptions :: TestOptions
defaultTestOptions =
  TestOptions
    { expectFailure = DontExpectFailure
    , overrideVerbose = Just Verbose
    , overrideMaxShrinks = Nothing
    , overrideNumTests = Just 10
    , overrideMaxRatio = Nothing
    }

demoProp :: (Show a) => String -> Gen a -> TestTree
demoProp label gen' = testPropertyWith defaultTestOptions label $ (gen gen') >>= collect label . pure

genResult :: Word -> Gen Result
genResult passPcnt =
  frequency
    [ (passPcnt, pure Pass)
    , (100 - passPcnt, pure Fail)
    ]

demoResult :: TestTree
demoResult = demoProp "result" $ genResult 80

genDelay :: Int -> Gen Int
genDelay maxms = inRange $ skewedBy 2 (0, maxms)

demoDelay :: TestTree
demoDelay = demoProp "delay" $ genDelay 3000

genSpec :: Int -> Word -> Gen Spec
genSpec maxDelay passPcnt = Spec <$> genDelay maxDelay <*> genResult passPcnt

demoSpec :: TestTree
demoSpec = demoProp "spec" $ genSpec 3000 80

data GenParams = GenParams
  { genStrategy :: SpecGen
  , maxTests :: Word
  , maxBranches :: Word
  , maxDelay :: Int
  , passPcnt :: Word
  , hookPassPcnt :: Word
  , maxDepth :: Word
  , minHz :: Hz
  }

genNode :: GenParams -> Gen Template
genNode
  gl@GenParams
    { genStrategy
    , maxDepth
    , minHz
    , maxDelay
    , maxBranches
    , maxTests
    , hookPassPcnt
    , passPcnt
    } =
    frequency
      [ (fixtureWeight, genFixture)
      , (eachWeight, genEachBefore)
      , (eachWeight, genEachAfter)
      , (eachWeight, genEachAround)
      , (onceWeight, genOnceBefore)
      , (onceWeight, genOnceAfter)
      , (onceWeight, genOnceAround)
      , (threadWeight, genThreadBefore)
      , (threadWeight, genThreadAfter)
      , (threadWeight, genThreadAround)
      ]
   where
    hkWeight = maxDepth < 2 ? 0 $ 10
    onceWeight = minHz > Once ? 0 $ hkWeight
    threadWeight = minHz > Thread ? 0 $ hkWeight
    eachWeight = hkWeight
    fixtureWeight = 100 - (onceWeight + threadWeight + eachWeight) * 2
    nxtLimits = gl{maxDepth = maxDepth - 1}
    genSubnodes = list (between (1, maxBranches))
    genOnceSubnodes = genSubnodes $ genNode (nxtLimits{minHz = Once})
    genSpec' = genSpec maxDelay passPcnt
    genSpec'' = genSpec maxDelay passPcnt
    genOnceBefore = OnceBefore <$> genSpec' <*> genOnceSubnodes
    genOnceAfter = OnceAfter <$> genSpec' <*> genOnceSubnodes
    genOnceAround = OnceAround <$> genSpec' <*> genSpec'' <*> genOnceSubnodes
    genThreadSubnodes = genSubnodes $ genNode (nxtLimits{minHz = Thread})
    genManySpec =
      frequency
        [ (10, T.All <$> genSpec')
        , (90, T.PassProb genStrategy (fromIntegral hookPassPcnt) 0 <$> genDelay maxDelay)
        ]
    genManySpec' =
      frequency
        [ (10, T.All <$> genSpec')
        , (90, T.PassProb genStrategy (fromIntegral hookPassPcnt) 0 <$> genDelay maxDelay)
        ]
    genThreadBefore = ThreadBefore <$> genManySpec <*> genThreadSubnodes
    genThreadAfter = ThreadAfter <$> genManySpec <*> genThreadSubnodes
    genThreadAround = ThreadAround <$> genManySpec <*> genManySpec' <*> genThreadSubnodes
    genEachSubnodes = genSubnodes $ genNode (nxtLimits{minHz = Each})
    genEachBefore = EachBefore <$> genManySpec <*> genEachSubnodes
    genEachAfter = EachAfter <$> genManySpec <*> genEachSubnodes
    genEachAround = EachAround <$> genManySpec <*> genManySpec' <*> genEachSubnodes
    genFixture = Fixture <$> (list (between (1, maxTests)) $ genSpec')

genParams :: GenParams
genParams =
  GenParams
    { genStrategy = Preload
    , maxTests = 20
    , maxBranches = 4
    , maxDelay = 1000
    , passPcnt = 90
    , hookPassPcnt = 95
    , maxDepth = 5
    , minHz = Once
    }

genTemplate :: GenParams -> Gen [Template]
genTemplate p = list (between (1, p.maxBranches)) $ genNode p

tryRunTest :: ThreadCount -> [Template] -> IO (Either SomeException ())
tryRunTest c suite =
  tryAny (runTest defaultSeed c suite)

-- https://hackage.haskell.org/package/base-4.19.1.0/docs/System-IO-Unsafe.html
{-# NOINLINE prop_test_suite #-}
prop_test_suite :: TestTree
prop_test_suite = 
  testPropertyWith defaultTestOptions "Template" $ do
    t <- genWith (Just . ppShow) $ genTemplate genParams
    let result = unsafePerformIO $ tryRunTest (ThreadCount 5) t
    assert $ FP.expect True `FP.dot` FP.fn ("is right", isRight) FP..$ ("t", result)

-- $ > test_suite
test_suite :: IO ()
test_suite = do
  print "RUNNING TEST"
  defaultMain $
    testGroup
      "generator stubs"
      [ --   demoResult
        -- , demoDelay
        -- , demoSpec
        prop_test_suite
      ]
