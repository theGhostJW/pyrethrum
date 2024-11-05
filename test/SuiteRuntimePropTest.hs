{-# OPTIONS_GHC -fno-cse #-}
-- https://hackage.haskell.org/package/base-4.19.1.0/docs/System-IO-Unsafe.html
{-# OPTIONS_GHC -fno-full-laziness #-}

module SuiteRuntimePropTest where

-- TODO review PyrethrumExtras.Test remove hedgehog in favour of falsify

import BasePrelude (unsafePerformIO)
import FullSuiteTestTemplate (Directive (..), Spec (..), SpecGen (..))
import FullSuiteTestTemplate qualified as T
import Internal.SuiteRuntime (ThreadCount (..))
import CoreUtils (Hz (..))
import PyrethrumExtras as PE
import SuiteRuntimeTestBase
import System.Random.Stateful qualified as RS
import Test.Falsify.Generator as G (Gen, frequency, inRange, list)
import Test.Falsify.Predicate qualified as FP
import Test.Falsify.Range (between, skewedBy)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify
  ( ExpectFailure (DontExpectFailure),
    TestOptions (..),
    -- Verbose (Verbose),
    assert,
    collect,
    gen,
    genWith,
    testPropertyWith,
  )
import Text.Show.Pretty (pPrint, ppShow)
import UnliftIO (tryAny)
import Prelude hiding (All, bug, id)

-- $ > genPlay

genPlay :: IO ()
genPlay = do
  i <- RS.uniformM RS.globalStdGen :: IO Int
  pPrint i
  rg <- RS.randomRIO (1, 100)
  pPrint rg

--  todo :: simple random api / effect

demoProp :: (Show a) => String -> Gen a -> TestTree
demoProp label gen' = testPropertyWith testOpts label $ gen gen' >>= collect label . pure

genResult :: Word -> Word -> Gen Directive
genResult passPcnt hookPassThroughErrPcnt =
  frequency
    [ (passPcnt, pure T.Pass),
      (100 - passPcnt - hookPassThroughErrPcnt, pure T.Fail),
      (hookPassThroughErrPcnt, pure PassThroughFail)
    ]

demoResult :: TestTree
demoResult = demoProp "result" $ genResult 80 5

genDelay :: Int -> Gen Int
genDelay maxms = inRange $ skewedBy 2 (0, maxms)

demoDelay :: TestTree
demoDelay = demoProp "delay" $ genDelay 3000

genHookSpec :: Int -> Word -> Word -> Gen Spec
genHookSpec maxDelay passPcnt hookPassThroughErrPcnt = Spec <$> genDelay maxDelay <*> genResult passPcnt hookPassThroughErrPcnt

demoSpec :: TestTree
demoSpec = demoProp "spec" $ genHookSpec 3000 80 4

data TGenParams = GenParams
  { genStrategy :: SpecGen,
    minTestsPerFixture :: Word,
    maxTestsPerFixture :: Word,
    maxBranches :: Word,
    maxDelay :: Int,
    passPcnt :: Word,
    hookPassPcnt :: Word,
    hookPassThroughErrPcnt :: Word,
    maxDepth :: Word,
    minHz :: Hz,
    threadCount :: ThreadCount,
    logging :: Logging,
    test :: Logging -> Int -> ThreadCount -> [Template] -> IO ()
  }

templateGenParams :: TGenParams -> Gen Template
templateGenParams
  gl@GenParams
    { genStrategy,
      maxDepth,
      minHz,
      maxDelay,
      maxBranches,
      minTestsPerFixture,
      maxTestsPerFixture,
      hookPassPcnt,
      hookPassThroughErrPcnt,
      passPcnt
    } =
    frequency
      [ (fixtureWeight, genFixture),
        (eachWeight, genEachBefore),
        (eachWeight, genEachAfter),
        (eachWeight, genEachAround),
        (onceWeight, genOnceBefore),
        (onceWeight, genOnceAfter),
        (onceWeight, genOnceAround),
        (threadWeight, genThreadBefore),
        (threadWeight, genThreadAfter),
        (threadWeight, genThreadAround)
      ]
    where
      hkWeight = maxDepth < 2 ? 0 $ 10
      onceWeight = minHz > Once ? 0 $ hkWeight
      threadWeight = minHz > Thread ? 0 $ hkWeight
      eachWeight = hkWeight
      fixtureWeight = 100 - (onceWeight + threadWeight + eachWeight) * 2
      nxtLimits = gl {maxDepth = maxDepth - 1}
      genSubnodes = G.list (between (1, maxBranches))
      genOnceSubnodes = genSubnodes $ templateGenParams (nxtLimits {minHz = Once})
      genSpec' = genHookSpec maxDelay passPcnt hookPassThroughErrPcnt
      genSpec'' = genHookSpec maxDelay passPcnt hookPassThroughErrPcnt
      genOnceBefore = OnceBefore <$> genSpec' <*> genOnceSubnodes
      genOnceAfter = OnceAfter <$> genSpec' <*> genOnceSubnodes
      genOnceAround = OnceAround <$> genSpec' <*> genSpec'' <*> genOnceSubnodes
      genThreadSubnodes = genSubnodes $ templateGenParams (nxtLimits {minHz = Thread})
      genManySpec =
        frequency
          [ (50, T.All <$> genSpec'),
            (50, T.PassProb genStrategy (fromIntegral hookPassPcnt) (fromIntegral hookPassThroughErrPcnt) 0 <$> genDelay maxDelay)
          ]
      genManySpec' =
        frequency
          [ (50, T.All <$> genSpec'),
            (50, T.PassProb genStrategy (fromIntegral hookPassPcnt) (fromIntegral hookPassThroughErrPcnt) 0 <$> genDelay maxDelay)
          ]
      genThreadBefore = ThreadBefore <$> genManySpec <*> genThreadSubnodes
      genThreadAfter = ThreadAfter <$> genManySpec <*> genThreadSubnodes
      genThreadAround = ThreadAround <$> genManySpec <*> genManySpec' <*> genThreadSubnodes
      genEachSubnodes = genSubnodes $ templateGenParams (nxtLimits {minHz = Each})
      genEachBefore = EachBefore <$> genManySpec <*> genEachSubnodes
      genEachAfter = EachAfter <$> genManySpec <*> genEachSubnodes
      genEachAround = EachAround <$> genManySpec <*> genManySpec' <*> genEachSubnodes
      genFixture = Fixture <$> G.list (between (minTestsPerFixture, maxTestsPerFixture)) genSpec'

defParams :: TGenParams
defParams =
  GenParams
    { genStrategy = Preload,
      minTestsPerFixture = 1,
      maxTestsPerFixture = 20,
      maxBranches = 4,
      maxDelay = 1000,
      passPcnt = 90,
      hookPassPcnt = 90,
      hookPassThroughErrPcnt = 2,
      maxDepth = 5,
      minHz = Once,
      threadCount = ThreadCount 5,
      logging = LogFails,
      test = runTest'
    }

genTemplate :: TGenParams -> Gen [Template]
genTemplate p = G.list (between (1, p.maxBranches)) $ templateGenParams p

tryRunTest :: TVar Bool -> TGenParams -> [Template] -> IO (Either SomeException ())
tryRunTest isShinking p suite = do
  r <- tryAny $ p.test p.logging defaultSeed p.threadCount suite
  srk <- readTVarIO isShinking
  let sfx = srk ? " (shrinking)" $ ""
  if isRight r
    then
      printNow $ "PASS" <> sfx
    else do
      atomically $ writeTVar isShinking True
      printNow $ "FAIL" <> sfx
      putStrLn "========="
  pure r

testOpts :: TestOptions
testOpts =
  TestOptions
    { expectFailure = DontExpectFailure,
      overrideVerbose = Nothing, -- Just Verbose
      overrideMaxShrinks = Nothing,
      overrideNumTests = Nothing, -- Just 10
      overrideMaxRatio = Nothing
    }

-- todo: add timestamp to debug
-- https://hackage.haskell.org/package/base-4.19.1.0/docs/System-IO-Unsafe.html
{-# NOINLINE runProp #-}
runProp ::  TVar Bool -> TestName -> TestOptions -> TGenParams -> TestTree
runProp isShrinking testName o p =
  testPropertyWith o testName $ do
    t <- genWith (Just . ppShow) $ genTemplate p
    let result = unsafePerformIO $ tryRunTest isShrinking p t
    assert $ FP.expect True `FP.dot` FP.fn ("is right", isRight) FP..$ ("t", result)

-- $> test_suite_preload
test_suite_preload :: IO ()
test_suite_preload = do
  -- need a separate shrinkState for every test group
  shrinkState <- newTVarIO False
  defaultMain $
    testGroup "PreLoad" [runProp shrinkState "Preload" testOpts {overrideNumTests = Just 1000} defParams {genStrategy = Preload}]

-- $ > test_suite_runtime
test_suite_runtime :: IO ()
test_suite_runtime = do
  -- need a separate shrinkState for every test group
  shrinkState <- newTVarIO False
  defaultMain $
    testGroup
      "generator stubs"
      [ runProp shrinkState "Runtime" testOpts {overrideNumTests = Just 100} defParams {genStrategy = Runtime, minTestsPerFixture = 1}
      ]

{- TODO: Check out performance.
  NResult threads is slower than a handfull
  Threads   Time for 100 tests (Seconds)
  --------------------------------------
  1         254
  5         109
  500       146

  could be contention on hook TMVars or child ques - or logging or memory
-}
