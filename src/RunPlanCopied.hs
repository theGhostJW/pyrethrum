
module RunPlanCopied where

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
import RunnerBase hiding (SuiteItem)

data HookFrequency = All | Each


data SuiteItem m a where 
  Plan :: Text -> [m a] -> SuiteItem m ()
  TestList :: [m a] -> SuiteItem m ()
  Test :: m a -> SuiteItem m ()
  Before :: {   
    freq :: HookFrequency,
    description :: Text,
    run :: m a,
    body :: m a 
    } -> SuiteItem m ()
  After :: {     
    freq :: HookFrequency,
    description :: Text,
    run :: m a,
    body :: m a 
    } -> SuiteItem m ()
  Label :: Text -> m () -> SuiteItem m ()
  Message :: Text -> SuiteItem m ()
  Notes :: Text -> Text -> m a -> SuiteItem m ()

makeSem ''SuiteItem

myTest :: Member SuiteItem effs => Sem effs ()
myTest = test $ message "sample test"

creditTests :: Member SuiteItem effs => Sem effs ()
creditTests = testList [myTest, myTest, myTest, myTest, myTest, myTest]

debitTests :: Member SuiteItem effs => Sem effs ()
debitTests = testList [myTest, myTest, myTest, myTest, myTest, myTest]

debitTestsInvalid :: Member SuiteItem effs => Sem effs ()
debitTestsInvalid = testList [myTest, myTest, myTest, myTest, myTest, myTest]

otherTests :: Member SuiteItem effs => Sem effs ()
otherTests = testList [myTest, myTest, myTest, myTest, myTest, myTest]

someMoreTests :: Member SuiteItem effs => Sem effs ()
someMoreTests = testList [myTest, myTest, myTest, myTest, myTest, myTest]

myPlan :: Member SuiteItem effs => Sem effs ()
myPlan = label "hello" $ message "Hello2"

logIn :: Member SuiteItem effs => Sem effs ()
logIn = message "dummy"

goToHomePage  :: Member SuiteItem effs => Sem effs ()
goToHomePage = message "dummy"

clearAddTrasactionForm :: Member SuiteItem effs => Sem effs ()
clearAddTrasactionForm = message "dummy"

openAddTransactionForm  :: Member SuiteItem effs => Sem effs ()
openAddTransactionForm = message "dummy"

ensureStillLoggedIn  :: Member SuiteItem effs => Sem effs ()
ensureStillLoggedIn = message "dummy"

closeDown  :: Member SuiteItem effs => Sem effs ()
closeDown = message "dummy"

goTransactionsPage  :: Member SuiteItem effs => Sem effs ()
goTransactionsPage = message "dummy"


myRun :: Member SuiteItem effs => Sem effs ()
myRun = suite "ACME Tests" [
                
                before All "login" logIn  $
                after Each "ensure logged in" ensureStillLoggedIn  $
                after All "close down" closeDown  $
                suite "debit" [
                  label "valid" debitTests,
                  label "invalid" debitTestsInvalid
                ], 
                  
                label "a nested suite example" $
                before All "login" logIn  $
                after Each "ensure logged in" ensureStillLoggedIn  $
                after All "close down" closeDown  $
                suite "credit" [
                    suite "nested suite example" [
                      otherTests,
                      someMoreTests
                    ]
                ]
             ]

-- An interpreter 