module RunnerBase where

import DSL.Ensure
import DSL.Common (FilterError)
import ItemClass
import Foundation.Extended
import  Control.Monad.Freer
import TestAndRunConfig
import Data.Aeson.TH
import Data.Aeson

-- this result is ultimately serialsed to JSON as part of the log protocol data  
-- type and can't serialise with custom typeclass constraints so forced to
-- have the redundant testModAddress and testTitle even though this
-- data is available via TestConfigClass
data TestDisplayInfo tc = TestDisplayInfo {
  testModAddress :: TestModule,
  testTitle :: String,
  testConfig :: tc
}  

deriving instance (Show tc) => Show (TestDisplayInfo tc) 
deriving instance (Eq tc) => Eq (TestDisplayInfo tc)

$(deriveJSON defaultOptions ''TestDisplayInfo)

mkDisplayInfo :: TestConfigClass tc => tc -> TestDisplayInfo tc
mkDisplayInfo tc = TestDisplayInfo {
                                    testModAddress = moduleAddress tc,
                                    testTitle = title tc,
                                    testConfig = tc
                                   }

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

data GenericTest tc rc i effs as vs = GenericTest {
  configuration :: tc,
  components :: ItemClass i vs => TestComponents rc i effs as vs
}