
{-# LANGUAGE RankNTypes  #-}

module SharedInterpreter2 where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import           Control.Monad.Freer.Reader
import           Foundation.Extended

newtype RunConfig = RunConfig {dummyProp :: String}
newtype TestConfig = TestConfig {dummyPropT :: String}

data GenericTest testConfig runConfig item effs apState valState =  GenericTest {
  address       :: String,
  configuration :: testConfig,
  components    :: TestComponents runConfig item effs apState valState
}

data TestComponents runConfig item apState effs valState = TestComponents {
  testItems      :: [item],
  testInteractor :: runConfig -> item -> effs,
  testPrepState  :: apState -> valState
}

type Test = GenericTest RunConfig TestConfig

test1 :: Members '[Writer String, Reader Int, Error String] effs => Test String (Eff effs String) String String
test1 = undefined

test2 :: Members '[Reader Int] effs => Test Int (Eff effs Int) Int Int
test2 = undefined

runAllFull :: forall i as vs. Test i (Eff '[Writer String, Reader Int, Error String, IO] as) as vs -> IO ()
runAllFull = undefined

mergeIO :: [IO ()] -> IO ()
mergeIO = foldl' (>>) (pure ())

runTest :: IO ()
runTest =
   mergeIO [
      runAllFull test1,
      runAllFull test2
    ]

-- testRunner :: (forall i as vs effs. Members '[Writer String, Reader Int, Error String] effs => Test i (Eff effs as) as vs -> IO ()) -> IO ()
-- testRunner f = mergeIO [
--                         f test1,
--                         f test2
--                       ]
--
-- --   -- intended use
-- runTest' :: IO ()
-- runTest' = testRunner runAllFull
