{-# LANGUAGE RankNTypes #-}

module TestTypes where

import           Foundation

newtype RunConfig = RunConfig {dummyProp :: String}
newtype TestConfig = TestConfig {dummyPropT :: String}

data GenericTest testConfig runConfig item effs apState dState = GenericTest {
  address       :: String,
  configuration :: testConfig,
  components    :: TestComponents runConfig item effs apState dState
}

data TestComponents runConfig item effs apState dState = TestComponents {
  testItems      :: [item],
  testInteractor :: runConfig -> item -> effs,
  testPrepState  :: apState -> dState
}

type Test = GenericTest RunConfig TestConfig
