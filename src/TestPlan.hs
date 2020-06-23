
module TestPlan where

import Data.Aeson
import Common
import           Pyrelude as P
import           Pyrelude.IO as PO
import           Control.Exception as E
import           DSL.Logger
import           DSL.LogProtocol
import Polysemy
import Polysemy.Error as PE
import RunElementClasses
import RunnerBase

{-
type Ensurable e effs = Members '[Ensure, Error (FrameworkError e)] effs

data GenericResult tc rslt = TestResult {
  configuration :: tc,
  results :: Either FilterErrorType [rslt]
} deriving Show

data PreRun effs = PreRun {
  runAction :: Sem effs (),
  checkHasRun :: Sem effs Bool
}

data TestGroup m1 m a effs =
  TestGroup {
        header :: Text,
        -- occurs once on client before group is run
        rollover :: PreRun effs,
        -- occurs once before test iteration is run
        goHome :: PreRun effs,
        -- a list of tests
        tests :: [m1 (m a)]
        -- eg [IO Either (FrameworkError TestInfo)]
   }

instance Titled (TestGroup m1 m a effs) where
  title = header

data TestComponents e rc i as ds effs = TestComponents {
  testItems :: rc -> [i],
  testInteractor :: rc -> i -> Sem effs as,
  testPrepState :: forall psEffs. (Ensurable e) psEffs => i -> as -> Sem psEffs ds
}

data GenericTest e tc rc i as ds effs = GenericTest {
  configuration :: tc,
  components :: ItemClass i ds => TestComponents e  rc i as ds effs
}

-- parametrised so can inject different preruns for testing
validPlan :: forall m m1 effs a. EFFAllEffects effs =>
  PreRun effs      -- rollOver0
  -> PreRun effs   -- goHome0
  -> PreRun effs   -- rollOver1
  -> PreRun effs   -- goHome1
  ->  TestPlan m1 m a effs
validPlan ro0 gh0 ro1 gh1 f =
  [

    TestGroup {
           header = "Group 1",
           rollover = ro0,
           goHome = gh0,
           tests = [
               f RT.test,
               f DT.test,
               f ST.test,
               f DemoProject.Test.RoughIntState.test
             ]
      },

    TestGroup {
          header = "Group 2",
          rollover = ro1,
          goHome = gh1,
          tests = [
              f RT2.test,
              f ST2.test
            ]
     }
    ]

simplePlan :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
simplePlan = validPlan doNothing doNothing doNothing doNothing


doNothing :: PreRun effs
doNothing = PreRun {
  runAction = pure (),
  checkHasRun = pure True
}

-}


-- from runner
type TestPlanBase e tc rc m1 m a effs = (forall i as ds. (ItemClass i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) => 
                                                          GenericTest e tc rc i as ds effs -> m1 (m a)) -> [TestGroup m1 m a effs]

data TestPlan m r where
  ReadFile :: Path a File -> TestPlan m Text
  WriteFile :: Path a File -> Text -> TestPlan m ()

makeSem ''TestPlan