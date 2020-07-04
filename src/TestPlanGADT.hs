
module TestPlanGADT where

import Data.Aeson
import Common
import           Pyrelude as P hiding (First)
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
validPlan :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
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

  -- tests :: [m1 (m a)]
  --       -- eg [IO Either (FrameworkError TestInfo)]

-- data TestPlan m r where
--   TestList :: Path a File -> TestPlan m Text
--   WriteFile :: Path a File -> Text -> TestPlan m ()

-- makeSem ''TestPlan

-- data TestGroup m1 m a effs =
--   TestGroup {
--         header :: Text,
--         -- occurs once on client before group is run
--         rollover :: PreRun effs,
--         -- occurs once before test iteration is run
--         goHome :: PreRun effs,
--         -- a list of tests
--         tests :: [m1 (m a)]
--         -- eg [IO Either (FrameworkError TestInfo)]
--    }

data HookPos = Before | After
data HookFrequency = All | Each


-- data FileSystem m r where
--   ReadFile :: Path a File -> FileSystem m Text
--   WriteFile :: Path a File -> Text -> FileSystem m ()

-- makeSem ''FileSystem

data TestPlan m1 m a where 
  SubPlan :: Text -> [TestPlan m1 m a] -> TestPlan m1 m ()
  Test :: m1 m a -> TestPlan m1 m ()
  Hook :: Text -> m1 m a -> HookPos -> HookFrequency -> TestPlan m1 m a -> TestPlan m1 m ()
  Label :: Text -> TestPlan m1 m a -> TestPlan m1 m ()
  Notes :: Text -> Text -> TestPlan m1 m a -> TestPlan m1 m ()

makeSem ''TestPlan

-- myTest :: IO (Either Text Int) = uu

-- myPlan :: TestPlan IO (Either Text) ()
-- myPlan = do 
        




-- logIn :: IO (Either Text Int) = uu
-- goToHomepage :: IO (Either Text Int) = uu
-- clearAddTrasactionForm:: IO (Either Text Int) = uu
-- openAddTransactionForm :: IO (Either Text Int) = uu
-- ensureStillLoggedIn :: IO (Either Text Int) = uu
-- closeDown :: IO (Either Text Int) = uu
-- goTransactionsPage :: IO (Either Text Int) = uu

-- testList = Test <$> [myTest, myTest, myTest, myTest]
-- postTestList = Test <$> [myTest, myTest, myTest]
-- creditTests = Test <$> [myTest, myTest, myTest]
-- debitTests = Test <$> [myTest, myTest, myTest]
-- debitTestsInvalid = Test <$> [myTest, myTest, myTest]



-- myPlan = 
--   Label "ACME Tests" $
--   Hook "login" logIn Before All $
--   Hook "ensure logged in" ensureStillLoggedIn After Each $
--   Hook "close down" closeDown After All $

--     Label "Transaction Control" $
--     Hook "go trans page" goTransactionsPage Before Each $
--     Hook "clear transactions" clearAddTrasactionForm After All $
--       SubPlan $ 
--         Label "Credit" (
--         Hook "open transaction form" openAddTransactionForm Before Each $
--         Hook "clear transaction form" clearAddTrasactionForm After All $
--         SubPlan creditTests
--       ) :

--     Label "Debit" (
--     Hook "go pay anyone" openAddTransactionForm Before Each $
--     Hook "clear transaction form" clearAddTrasactionForm After All $
--     SubPlan [
--               Label "valid scenarios" (
--               SubPlan
--               debitTests),
              
--               Label "invalid scenarios" (
--               SubPlan
--               debitTests)
--             ]
--     ) :

--     [SubPlan (testList <> postTestList)]