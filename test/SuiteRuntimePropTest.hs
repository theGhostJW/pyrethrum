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
import Test.Tasty (TestTree, defaultMain, testGroup, TestName)
import Chronos (Time, now)
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


-- $ > genPlay
genPlay :: IO ()
genPlay = do
  i <- RS.uniformM RS.globalStdGen :: IO Int
  pPrint i
  rg <- RS.randomRIO (1, 100)
  pPrint rg

--  todo :: simple random api / effect

demoProp :: (Show a) => String -> Gen a -> TestTree
demoProp label gen' = testPropertyWith falsifyOptions label $ (gen gen') >>= collect label . pure

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

tryRunTest :: Logging -> ThreadCount -> [Template] -> IO (Either SomeException ())
tryRunTest wantLog c suite =
  tryAny (printNow "TEST START" >> runTest' wantLog defaultSeed c suite >> printNow "TEST END")


falsifyOptions :: TestOptions
falsifyOptions =
  TestOptions
    { expectFailure = DontExpectFailure
    , overrideVerbose = Nothing -- Just Verbose
    , overrideMaxShrinks = Nothing
    , overrideNumTests = Just 100
    , overrideMaxRatio = Nothing
    }

-- todo: add timestamp to debug
-- https://hackage.haskell.org/package/base-4.19.1.0/docs/System-IO-Unsafe.html
{-# NOINLINE runProp #-}
runProp :: TestName -> SpecGen -> TestTree
runProp testName genStrategy = 
  testPropertyWith falsifyOptions testName $ do
    t <- genWith (Just . ppShow) $ genTemplate genParams {genStrategy = genStrategy}
    let result = unsafePerformIO $ tryRunTest NoLog (ThreadCount 5)  t
    assert $ FP.expect True `FP.dot` FP.fn ("is right", isRight) FP..$ ("t", result)

putTxt :: (ConvertString a String) => a -> IO ()
putTxt = putStrLn . toS

printTime :: Text -> Time -> IO ()
printTime msg t = putTxt $ msg <> ":: " <> toS (show t)

printNow :: Text -> IO ()
printNow lbl = do 
  t <- now
  printTime lbl  t

-- $ > test_suite_preload
test_suite_preload :: IO ()
test_suite_preload = do
  defaultMain $
   testGroup "PreLoad" [runProp "Preload" Preload]
  print "TEST SUITE DONE"

-- $ > test_suite_runtime
test_suite_runtime :: IO ()
test_suite_runtime = do
  defaultMain $
    testGroup
      "generator stubs"
      [
        runProp "Runtime" Runtime
      ]
  print "TEST SUITE DONE"
