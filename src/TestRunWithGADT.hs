module TestRunWithGADT where

import Data.Aeson
import Common hiding (message)
import           Pyrelude as P hiding (First)
import           Pyrelude.IO as PO
import           Control.Exception as E
import           DSL.Logger
import           DSL.LogProtocol
import Polysemy
import Polysemy.Error as PE
import RunElementClasses hiding (whenClause, thenClause)
import RunnerBase hiding (tests, TestComponents, GenericTest)
import Check hiding (message)
import DSL.Interpreter

data HookFrequency = All | Each


-- data BaseTest rc tc e i as ds effs = BaseTest {
--   config :: tc,
--   items :: rc -> [i],
--   interactor :: rc -> i -> Sem effs as,
--   prepState :: forall effs0. (Ensurable e) effs0 => i -> as -> Sem effs0 ds
-- }
data BaseTest rc tc e i as ds m = BaseTest {
  config :: tc,
  items :: rc -> [i],
  interactor :: rc -> i -> m as,
  prepState :: i -> as -> Either e ds
}


data RunElement rc tc e m a where 
  Plan :: Text -> [m a] -> RunElement rc tc e m ()
  Tests :: [m a] -> RunElement rc tc e m ()
  Test :: BaseTest rc tc e i as ds m -> RunElement rc tc e m o
  Before :: {   
    freq :: HookFrequency,
    description :: Text,
    run :: m a,
    body :: m a 
    } -> RunElement rc tc e m ()
  After :: {     
    freq :: HookFrequency,
    description :: Text,
    run :: m a,
    body :: m a 
    } -> RunElement rc tc e m ()
  Label :: Text -> m () -> RunElement rc tc e m ()
  Message :: Text -> RunElement rc tc e m ()
  Notes :: Text -> Text -> m a -> RunElement rc tc e m ()

makeSem ''RunElement


-- testRunInterpreter :: forall rc tc e testEffs effs a. Members (TestIOEffects e) effs => rc -> Sem (RunElement rc tc e ': effs) a -> Sem effs [a]
-- testRunInterpreter =
--     interpret $ \case 
--                   Test bt -> _ 
--                   _ -> uu


-- runTest ::  forall i rc as ds tc e effs. (ItemClass i ds, TestConfigClass tc, ToJSON e, Show e, Member (Logger e) effs) =>
--                    Maybe (S.Set Int)                                                        -- target Ids
--                    -> FilterList rc tc                                                      -- filters
--                    -> (ItemParams e as ds i tc rc effs -> Sem effs ())                      -- item runner
--                    -> rc                                                                    -- runConfig
--                    -> GenericTest rc tc e i as ds effs                                     -- Test Case
--                    -> [Sem effs ()]                                                         -- [TestIterations]
-- runTest iIds fltrs itemRunner rc GenericTest {configuration = tc, components} = uu


-- #####################################################################################
-- ##################################### Example #######################################
-- #####################################################################################

-- myRun :: Member RunElement effs => Sem effs ()
-- myRun = 
--   plan "ACME Tests" [
--     before All "login" logIn  $
--     after Each "ensure logged in" ensureStillLoggedIn  $
--     after All "close down" closeDown  $
--     tests [
--       label "valid" debitTests,
--       label "invalid" debitTestsInvalid,
--       myTest
--     ], 
      
--     label "a nested plan example" $
--     before All "login" logIn  $
--     after Each "ensure logged in" ensureStillLoggedIn  $
--     after All "close down" closeDown  $
--     plan "credit" [
--         plan "nested plan example" [
--           otherTests,
--           someMoreTests
--         ]
--     ]
--   ]

-- myTest :: Member RunElement effs => Sem effs ()
-- myTest = test $ message "sample test"

-- creditTests :: Member RunElement effs => Sem effs ()
-- creditTests = tests [myTest, myTest, myTest, myTest, myTest, myTest]

-- debitTests :: Member RunElement effs => Sem effs ()
-- debitTests = tests [myTest, myTest, myTest, myTest, myTest, myTest]

-- debitTestsInvalid :: Member RunElement effs => Sem effs ()
-- debitTestsInvalid = tests [myTest, myTest, myTest, myTest, myTest, myTest]

-- otherTests :: Member RunElement effs => Sem effs ()
-- otherTests = tests [myTest, myTest, myTest, myTest, myTest, myTest]

-- someMoreTests :: Member RunElement effs => Sem effs ()
-- someMoreTests = tests [myTest, myTest, myTest, myTest, myTest, myTest]

-- myPlan :: Member RunElement effs => Sem effs ()
-- myPlan = label "hello" $ message "Hello2"

-- logIn :: Member RunElement effs => Sem effs ()
-- logIn = message "dummy"

-- goToHomePage  :: Member RunElement effs => Sem effs ()
-- goToHomePage = message "dummy"

-- clearAddTrasactionForm :: Member RunElement effs => Sem effs ()
-- clearAddTrasactionForm = message "dummy"

-- openAddTransactionForm  :: Member RunElement effs => Sem effs ()
-- openAddTransactionForm = message "dummy"

-- ensureStillLoggedIn  :: Member RunElement effs => Sem effs ()
-- ensureStillLoggedIn = message "dummy"

-- closeDown  :: Member RunElement effs => Sem effs ()
-- closeDown = message "dummy"

-- goTransactionsPage  :: Member RunElement effs => Sem effs ()
-- goTransactionsPage = message "dummy"


-- type TestSource rc tc e m1 m a effs = (forall i as ds. (ItemClass' i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) => BaseTest rc tc e effs i as ds -> m1 (m a))


-- class (ToJSON i, Generic i) => ItemClass' i ds | i -> ds  where
--   identifier :: i -> Int
--   whenClause :: i -> Text
--   thenClause :: i -> Text
--   checkList :: i -> CheckDList ds

--   whenThen :: i -> Text
--   whenThen i = "When: " <> whenClause i  <> "\n" <>
--                "Then: " <> thenClause i


-- type TestPlanBase rc tc e m1 m a effs = 
--     (forall i as ds. (ItemClass i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) => GenericTest rc tc e i as ds effs -> m1 (m a)) -> [TestGroup m1 m a effs]

-- type TestPlanBase rc tc e m1 m a effs = 
--     (forall i as ds. (ItemClass' i ds, Show i, Show as, Show ds, ToJSON as, ToJSON ds) 
--         => GenericTest rc tc e i as ds effs -> m1 (m a)) -> [TestGroup' m1 m a effs]