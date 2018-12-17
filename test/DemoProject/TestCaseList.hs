
module DemoProject.TestCaseList where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Writer
import Data.Functor.Identity
import           DemoProject.Config
import           DemoProject.Test.Rough as RT
import           DemoProject.Test.Rough2 as RT2
import           DemoProject.Test.Simple as ST
import Control.Monad
import           DemoProject.Test.Simple2 as ST2
import qualified Data.Set as S
import  Data.Functor (($>))
import           DSL.Ensure
import           DSL.FileSystem
import           DSL.Interpreter
import           DSL.Logger
import           Foundation.List.DList
import           Foundation.Extended
import qualified Prelude                    as P
import           Runner as R
import Control.Exception as E

runInIO :: IO ()
runInIO = testRun plan [] testInfoFull executeInIO runConfig

runNZInIO :: IO ()
runNZInIO = testRun plan filters testInfoFull executeInIO runConfig {country = NZ}

runDocument :: DList String
runDocument = extractDocLog $ testRun plan [] testInfoFull executeDocument runConfig

validPlan :: forall m m1 effs a. EFFFileSystem effs =>
  PreRun effs
  -> PreRun effs
  -> PreRun effs
  -> PreRun effs
  ->  TestPlan m1 m a effs
validPlan ro0 gh0 ro1 gh1 f =
  [

    TestGroup {
           rollover = ro0,
           goHome = gh0,
           tests = [
               f RT.test,
               f ST.test -- 6 iterations
             ]
      },

    TestGroup {
          rollover = ro1,
          goHome = gh1,
          tests = [
              f RT2.test,
              f ST2.test -- 6 iterations
            ]
     }

    ]

plan :: forall m m1 effs a. EFFFileSystem effs => TestPlan m1 m a effs
plan = validPlan doNothing doNothing doNothing doNothing


alwaysFailCheck :: PreRun effs
alwaysFailCheck = PreRun {
  runAction = pure (),
  checkHasRun = pure False
}

testRunFailHomeG2 :: forall m m1 effs a. EFFFileSystem effs => TestPlan m1 m a effs
testRunFailHomeG2 = validPlan doNothing doNothing doNothing alwaysFailCheck

runFailHomeG2IO :: IO ()
runFailHomeG2IO = testRun testRunFailHomeG2 [] testInfoFull executeInIO runConfig

runFailHomeG2Document :: DList String
runFailHomeG2Document = extractDocLog $ testRun testRunFailHomeG2 [] testInfoFull executeDocument runConfig

testRunFailRolloverG1 :: forall m m1 effs a. EFFFileSystem effs => TestPlan m1 m a effs
testRunFailRolloverG1 = validPlan alwaysFailCheck doNothing doNothing doNothing

runFailRolloverG1Document :: DList String
runFailRolloverG1Document = extractDocLog $ testRun testRunFailRolloverG1 [] testInfoFull executeDocument runConfig

runFailRolloverG1IO :: IO ()
runFailRolloverG1IO = testRun testRunFailRolloverG1 [] testInfoFull executeInIO runConfig

ioException :: Eff effs Bool
ioException = (E.throw $ P.userError "Pretend IO Error") :: Eff effs Bool

exceptionInCheck :: PreRun effs
exceptionInCheck = PreRun {
  runAction = pure (),
  checkHasRun = ioException
}

testRunFailExceptG2GoHomeCheck :: forall m m1 effs a. EFFFileSystem effs => TestPlan m1 m a effs
testRunFailExceptG2GoHomeCheck = validPlan doNothing doNothing doNothing exceptionInCheck

runExceptG2GoHomeCheckIO :: IO ()
runExceptG2GoHomeCheckIO = testRun testRunFailExceptG2GoHomeCheck [] testInfoFull executeInIO runConfig

exceptionInRollover :: PreRun effs
exceptionInRollover = PreRun {
  runAction = void ioException,
  checkHasRun = pure True
}

testRunExceptG1Rollover:: forall m m1 effs a. EFFFileSystem effs => TestPlan m1 m a effs
testRunExceptG1Rollover = validPlan exceptionInRollover doNothing doNothing doNothing

runExceptG1Rollover :: IO ()
runExceptG1Rollover = testRun testRunExceptG1Rollover [] testInfoFull executeInIO runConfig

justLogPreRun :: EFFLogger effs => PreRun effs
justLogPreRun = PreRun {
  runAction = log "Run Action",
  checkHasRun = log "check Action Run" $> True
}
