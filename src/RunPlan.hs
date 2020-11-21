
module RunPlan where

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
import RunnerBase hiding (RunElement)
import qualified Data.Aeson as A

data HookFrequency = All | Each
data HookPosition = Before | After

-- Can't get bind to work in test plan
-- data RunElement m a where 
--   Hook :: { 
--     input :: i,  
--     pos :: HookPosition,
--     freq :: HookFrequency,
--     description :: Text,
--     executeHook :: i -> RunElement m b,
--     body :: b -> RunElement m ()
--     } -> RunElement m a
    
--   TestList :: b -> [m a] -> RunElement m ()

--   Test :: m a -> RunElement m ()

--   Message :: Text -> RunElement m ()


-- freaky higher order effects type errors on test plan
data RunElement m a where 
  Hook :: { 
    input :: i,  
    pos :: HookPosition,
    freq :: HookFrequency,
    description :: Text,
    executeHook :: i -> m b,
    body :: b -> m ()
    } -> RunElement m a
    
  TestList :: b -> [m a] -> RunElement m ()

  Test :: m a -> RunElement m ()

  Message :: Text -> RunElement m ()

makeSem ''RunElement

creditTests :: Member RunElement effs => a -> Sem effs ()
creditTests a = testList a [
                    message "sample test",
                    test $ message "message 2",
                    message "sample test",
                    message "sample test",
                    test $ message "message 2",
                    message "sample test",
                    message "sample test"
                  ]


debitTests :: Member RunElement effs => Int -> Sem effs ()
debitTests i = testList i [
                    message "sample test",
                    test $ message "message 2",
                    message "sample test",
                    message "sample test",
                    test $ message "message 2",
                    message "sample test",
                    message "sample test"
                  ]

debitGroup :: Member RunElement ceffs => Sem ceffs (Sem effs ())
debitGroup = hook
                1
                Before
                All
                "credit and debit tests"
                pure
                debitTests

creditGroup :: forall a effs. (Member RunElement effs) => a -> Sem effs ()
creditGroup a = hook 
                a
                Before
                Each
                "credit and debit tests"
                pure
                creditTests


logIn :: Member RunElement effs => Sem effs ()
logIn = test $ message "hello"

testRun :: Member RunElement effs => Sem effs ()
testRun = do 
           debitGroup
           creditGroup "Hello"
           logIn
           pure ()

-- An interpreter 

-- runElementDocInterpreter :: forall a effs. Member RunElement effs => Sem (RunElement ': effs) a -> Sem effs a
-- runElementDocInterpreter = 
--     interpretH $  \case 
--       Hook input pos freq description executeHook body -> 
--         do 
--           message description
--           let 
--             b =  executeHook input >>= body
--           pure uu

--       _ -> uu



