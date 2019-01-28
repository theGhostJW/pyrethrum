module RunnerBase where

import DSL.Ensure
import DSL.Common (FilterError)
import ItemClass
import Foundation.Extended
import  Control.Monad.Freer
import TestAndRunConfig

newtype ModuleAddress = ModuleAddress String
newtype TestTitle = TestTitle String

data GenericResult tc rslt = TestResult {
  configuration :: tc,
  results :: Either FilterError [rslt]
} deriving Show

data PreRun effs = PreRun {
  runAction :: Eff effs (),
  checkHasRun :: Eff effs Bool
}

data TestGroup m1 m a effs =
  TestGroup {
        header :: String,
        -- occurs once on client before group is run
        rollover :: PreRun effs,
        -- occurs once before test iteration is run
        goHome :: PreRun effs,
        -- a list of tests
        tests :: [m1 (m a)]
        -- eg [IO Either (AppError TestInfo)]
   }

instance Titled (TestGroup m1 m a effs) where
  title = header

data TestComponents rc i effs as vs = TestComponents {
  testItems :: [i],
  testInteractor :: rc -> i -> Eff effs as,
  testPrepState :: as -> Ensurable vs
}

data  GenericTest tc rc i effs as vs = GenericTest {
  configuration :: tc,
  components :: ItemClass i vs => TestComponents rc i effs as vs
}
  