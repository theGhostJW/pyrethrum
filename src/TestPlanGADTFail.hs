module TestPlanGADTFail where

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

data HookPos = Before | After
data HookFrequency = All | Each


data TestPlan m a where 
  SubPlan :: [TestPlan m a] -> TestPlan m ()
  Test :: m a -> TestPlan m ()
  Hook :: { 
    description :: Text,
    run :: m a,
    pos :: HookPos,
    freq :: HookFrequency,
    plan :: TestPlan m a 
    } -> TestPlan m ()
  Label :: Text -> TestPlan m () -> TestPlan m ()
  Message1 :: Text -> TestPlan m ()
  Notes :: Text -> Text -> TestPlan m a -> TestPlan m ()

makeSem ''TestPlan


myRun :: forall effs. Member TestPlan effs => Sem effs ()
myRun = label "hello" $ Message1 "Hello2"