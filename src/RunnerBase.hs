module RunnerBase where

import DSL.Ensure
import Common (FilterErrorType, FrameworkError)
import Pyrelude
import Polysemy
import Polysemy.Error
import RunElementClasses
import Data.Aeson

type ItemRunner e as ds i tc rc effs = 
    TestParams e as ds i tc rc effs -> i -> Sem effs ()

type TestRunner e tc rc m1 m a effs = (forall i as ds. (ItemClass i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) => 
                                                          GenericTest e tc rc i as ds effs -> m1 (m a)) 

type TestPlan e tc rc m1 m a effs = TestRunner e tc rc m1 m a effs -> [RunElement m1 m a effs]

type Ensurable e effs = Members '[Ensure, Error (FrameworkError e)] effs

data GenericResult tc rslt = TestResult {
  configuration :: tc,
  results :: Either FilterErrorType [rslt]
} deriving Show

data PreRun effs = PreRun {
  runAction :: Sem effs (),
  checkHasRun :: Sem effs Bool
}

doNothing :: PreRun effs
doNothing = PreRun {
  runAction = pure (),
  checkHasRun = pure True
}


data RunElement m1 m a effs =
  Tests {
        header :: Text,
        -- occurs once on client before group is run
        rollover :: PreRun effs,
        -- occurs once before test iteration is run
        goHome :: PreRun effs,
        -- a list of tests
        tests :: [m1 (m a)]
        -- eg [IO Either (FrameworkError TestInfo)]
   }

instance Titled (RunElement m1 m a effs) where
  title = header

data TestParams e as ds i tc rc effs = TestParams {                       
  interactor :: rc -> i -> Sem effs as,                         
  prepState :: forall pEffs. (Ensurable e) pEffs => i -> as -> Sem pEffs ds,                      
  tc :: tc,                                                     
  rc :: rc                                                      
}

data GenericTest e tc rc i as ds effs = GenericTest {
  config :: tc,
  testItems :: rc -> [i],
  testInteractor :: rc -> i -> Sem effs as,
  testPrepState :: forall psEffs. (Ensurable e) psEffs => i -> as -> Sem psEffs ds
}