module RunnerBase where

import DSL.Ensure
import Common (FilterErrorType, FrameworkError)
import Pyrelude
import Polysemy
import Polysemy.Error
import RunElementClasses
import Data.Aeson

type ItemRunner e as ds i tc rc effs = 
    rc -> Test e tc rc i as ds effs -> i -> Sem effs ()

type TestPlanBase e tc rc m1 m a effs = (forall i as ds. (ItemClass i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) => 
                                                          Test e tc rc i as ds effs -> m1 (m a)) -> [RunElement m1 m a effs]

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

data HookLocation = BeforeAll | 
                    AfterAll | 
                    BeforeEach |
                    AfterEach

{- RunElement Update
  1. Add Hook
  2. Demo Test Run
  2. Thread Hook Output to Subelements (GADT)
  3. Update Demo
  4. Concurrency
  5. Update Demo
  6. Runner that extracts test items - eg to report knowne errors
-}

data RunElement m1 m a effs =
  Tests {
        -- a list of tests
        tests :: [m1 (m a)]
        -- eg [IO Either (FrameworkError TestInfo)]
   } |

   Hook {
     location :: HookLocation,
     hook :: Sem effs (),
     subTests :: [RunElement m1 m a effs]
   } |

   Group {
    title :: Text,
    subTests :: [RunElement m1 m a effs]
   }


-- data RunElement m1 m a effs =
--   Tests {
--         header :: Text,
--         -- occurs once on client before group is run
--         rollover :: PreRun effs,
--         -- occurs once before test iteration is run
--         goHome :: PreRun effs,
--         -- a list of tests
--         tests :: [m1 (m a)]
--         -- eg [IO Either (FrameworkError TestInfo)]
--    } 

-- instance Titled (RunElement m1 m a effs) where
--   title = header

data Test e tc rc i as ds effs = Test {
  config :: tc,
  items :: rc -> [i],
  interactor :: rc -> i -> Sem effs as,
  prepState :: forall psEffs. (Ensurable e) psEffs => i -> as -> Sem psEffs ds
}