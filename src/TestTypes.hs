{-# LANGUAGE RankNTypes #-}

module TestTypes where

import           Foundation

newtype RunConfig = RunConfig {dummyProp :: String}
newtype TestConfig = TestConfig {dummyPropT :: String}

data GenericTest testConfig runConfig item effs apState valState = GenericTest {
  address       :: String,
  configuration :: testConfig,
  components    :: TestComponents runConfig item effs apState valState
}

data TestComponents runConfig item effs apState valState = TestComponents {
  testItems      :: [item],
  testInteractor :: runConfig -> item -> effs,
  testPrepState  :: apState -> valState
}

type Test = GenericTest RunConfig TestConfig
