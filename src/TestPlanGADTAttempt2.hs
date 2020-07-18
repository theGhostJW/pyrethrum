module TestPlanGADTAttempt2 where

import Data.Aeson
import Common hiding (message)
import           Pyrelude as P hiding (First)
import           Pyrelude.IO as PO
import           Control.Exception as E
import           DSL.Logger
import           DSL.LogProtocol
import Polysemy
import Polysemy.Error as PE
import RunElementClasses
import RunnerBase

data HookFrequency = All | Each


data RunElement m a where 
  Plan :: Text -> [m a] -> RunElement m ()
  TestList :: [m a] -> RunElement m ()
  Test :: m a -> RunElement m ()
  Before :: {   
    freq :: HookFrequency,
    description :: Text,
    run :: m a,
    body :: m a 
    } -> RunElement m ()
  After :: {     
    freq :: HookFrequency,
    description :: Text,
    run :: m a,
    body :: m a 
    } -> RunElement m ()
  Label :: Text -> m () -> RunElement m ()
  Message :: Text -> RunElement m ()
  Notes :: Text -> Text -> m a -> RunElement m ()

makeSem ''RunElement

myTest :: Member RunElement effs => Sem effs ()
myTest = test $ message "sample test"

creditTests :: Member RunElement effs => Sem effs ()
creditTests = testList [myTest, myTest, myTest, myTest, myTest, myTest]

debitTests :: Member RunElement effs => Sem effs ()
debitTests = testList [myTest, myTest, myTest, myTest, myTest, myTest]

debitTestsInvalid :: Member RunElement effs => Sem effs ()
debitTestsInvalid = testList [myTest, myTest, myTest, myTest, myTest, myTest]

otherTests :: Member RunElement effs => Sem effs ()
otherTests = testList [myTest, myTest, myTest, myTest, myTest, myTest]

someMoreTests :: Member RunElement effs => Sem effs ()
someMoreTests = testList [myTest, myTest, myTest, myTest, myTest, myTest]

myPlan :: Member RunElement effs => Sem effs ()
myPlan = label "hello" $ message "Hello2"

logIn :: Member RunElement effs => Sem effs ()
logIn = message "dummy"

goToHomePage  :: Member RunElement effs => Sem effs ()
goToHomePage = message "dummy"

clearAddTrasactionForm :: Member RunElement effs => Sem effs ()
clearAddTrasactionForm = message "dummy"

openAddTransactionForm  :: Member RunElement effs => Sem effs ()
openAddTransactionForm = message "dummy"

ensureStillLoggedIn  :: Member RunElement effs => Sem effs ()
ensureStillLoggedIn = message "dummy"

closeDown  :: Member RunElement effs => Sem effs ()
closeDown = message "dummy"

goTransactionsPage  :: Member RunElement effs => Sem effs ()
goTransactionsPage = message "dummy"


myRun :: Member RunElement effs => Sem effs ()
myRun = plan "ACME Tests" [
                
                before All "login" logIn  $
                after Each "ensure logged in" ensureStillLoggedIn  $
                after All "close down" closeDown  $
                plan "debit" [
                  label "valid" debitTests,
                  label "invalid" debitTestsInvalid
                ], 
                  
                label "a nested plan example" $
                before All "login" logIn  $
                after Each "ensure logged in" ensureStillLoggedIn  $
                after All "close down" closeDown  $
                plan "credit" [
                    plan "nested plan example" [
                      otherTests,
                      someMoreTests
                    ]
                ]
             ]