
module TestPlanSimple where

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

data TestPlan m1 m a = 
  SubPlan [TestPlan m1 m a] |
  Test (m1 (m a)) |
  Hook {
    description :: Text,
    run :: m1 (m a),
    pos :: HookPos,
    freq :: HookFrequency,
    plan :: TestPlan m1 m a
  } |
  Label {
    title :: Text, 
    plan :: TestPlan m1 m a
  } |
  Notes {
    title :: Text, 
    notes :: Text, 
    plan :: TestPlan m1 m a
    }

myTestC :: IO (Either Text Int) = uu
logInC :: IO (Either Text Int) = uu
goToHomepageC :: IO (Either Text Int) = uu
clearAddTrasactionFormC :: IO (Either Text Int) = uu
openAddTransactionFormC :: IO (Either Text Int) = uu
ensureStillLoggedInC :: IO (Either Text Int) = uu
closeDownC :: IO (Either Text Int) = uu
goTransactionsPageC :: IO (Either Text Int) = uu

testListC = Test <$> [myTestC, myTestC, myTestC, myTestC]
postTestListC = Test <$> [myTestC, myTestC, myTestC]
creditTestsC = Test <$> [myTestC, myTestC, myTestC]
debitTestsC = Test <$> [myTestC, myTestC, myTestC]
debitTestsInvalidC = Test <$> [myTestC, myTestC, myTestC]


myPlan :: TestPlan IO (Either Text) Int
myPlan = 
  Label "ACME Tests" $
  Hook "login" logInC Before All $
  Hook "ensure logged in" ensureStillLoggedInC After Each $
  Hook "close down" closeDownC After All $

    Label "Transaction Control" $
    Hook "go trans page" goTransactionsPageC Before Each $
    Hook "clear transactions" clearAddTrasactionFormC After All $
      SubPlan $ 
        Label "Credit" (
        Hook "open transaction form" openAddTransactionFormC Before Each $
        Hook "clear transaction form" clearAddTrasactionFormC After All $
        SubPlan creditTestsC
      ) :

      Label "Debit" (
      Hook "go pay anyone" openAddTransactionFormC Before Each $
      Hook "clear transaction form" clearAddTrasactionFormC After All $
      SubPlan [
                Label "valid scenarios" $
                SubPlan debitTestsC,
                
                Label "invalid scenarios" $
                SubPlan debitTestsC
              ]
    ) :

    [SubPlan (testListC <> postTestListC)]