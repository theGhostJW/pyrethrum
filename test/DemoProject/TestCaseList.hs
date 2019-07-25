
module DemoProject.TestCaseList where

import           Control.Monad.Freer
import           DemoProject.Config
import           DemoProject.Test.Rough as RT
import           DemoProject.Test.Rough2 as RT2
import           DemoProject.Test.Simple as ST
import DemoProject.Test.RoughIntState
import DemoProject.Test.RoughDisabled as DT
import  DemoProject.Test.Simple2 as ST2
import Control.Monad
import  Data.Functor (($>))
import           Common
import           DSL.Interpreter
import           DSL.Logger
import           DSL.Ensure
import DSL.LogProtocol
import DSL.LogProtocol.PrettyPrint
import           Data.DList
import           Pyrelude as P
import           Pyrelude.IO as PIO 
import           Runner as R
import qualified System.IO as S
import qualified Control.Exception as E
import AuxFiles as A
import LogTransformation (prepareFinalLogs)
import Data.Aeson (ToJSON(..))
import Data.Map as M

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


ioRun :: (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
ioRun pln = testRun pln filters normalExecution executeInIOConsolePretty runConfig

ioRunRaw :: (forall m1 m a. TestPlan m1 m a FullIOEffects) -> IO ()
ioRunRaw pln = testRun pln filters normalExecution executeInIOConsoleRaw runConfig

data WantConsole = Console | NoConsole deriving Eq

jsonItemLogExt = ".jsoni"

-- TODO: move to library file 
ioRunToFile :: 
    WantConsole
    -> Bool 
    -> (forall m1 m a. TestPlan m1 m a FullIOEffects) 
    -> (forall a.
          (forall (effs :: [* -> *]).
          LastMember IO effs =>
          Eff (Logger : effs) ~> Eff effs)
          -> Eff FullIOEffects a -> IO (Either AppError a)
    )
    -> (
        forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => 
          (LogProtocol -> IO ()) 
          -> (RunConfig -> i -> Eff FullIOEffects as) 
          -> (as -> Ensurable ds) 
          -> (forall a. Eff FullIOEffects a -> IO (Either AppError a))  
          -> TestConfig -> RunConfig 
          -> i 
          -> IO ()
       ) 
     -> IO (Either AppError [AbsFile])
ioRunToFile wantConsole docMode pln interpt itemRunner = 
  let 
    handleSpec :: M.Map (Text, FileExt) (LogProtocol -> Text) 
    handleSpec = M.fromList [
                                (("raw", FileExt ".log"), prettyPrintLogProtocol docMode)
                              , (("raw", FileExt jsonItemLogExt), logStrJSON)
                            ]

    fileHandleInfo :: IO (Either AppError [(LogProtocol -> Text, HandleInfo)])
    fileHandleInfo = logFileHandles handleSpec

    printFilePaths :: [AbsFile] -> IO ()
    printFilePaths lsFiles = do 
                                putStrLn ""
                                putStrLn "--- Log Files ---"
                                sequence_ $ putStrLn . toS . toFilePath <$> lsFiles
                                putStrLn ""
                          
    fileHandles :: IO (Either AppError [(Maybe AbsFile, LogProtocol -> Text, S.Handle)])
    fileHandles = (((\(fn, fh) -> (Just $ A.path fh, fn, fileHandle fh)) <$>) <$>) <$> fileHandleInfo

    closeFileHandles :: [S.Handle] -> IO ()
    closeFileHandles hdls = sequence_ $ S.hClose <$> hdls

    allHandles :: IO (Either AppError [(Maybe AbsFile, LogProtocol -> Text, S.Handle)])
    allHandles = wantConsole == Console
                      ? (((Nothing, prettyPrintLogProtocol docMode, S.stdout) :) <$>) <$> fileHandles
                      $ fileHandles
    
    runTheTest :: [(LogProtocol -> Text, S.Handle)] -> IO ()
    runTheTest targHndls = testRun pln filters itemRunner (interpt (logToHandles targHndls)) runConfig
  in 
    do 
      hndls <- allHandles
      eitherf hndls
        (pure . Left)
        (\hList -> 
          do 
            let 
              getFile (mfile, _, _)  = mfile
              fileHndls = P.filter (isJust . getFile) hList
              logPths = catMaybes $ getFile <$> fileHndls
            runTheTest ((\(af, fn, h) -> (fn, h)) <$> hList) `finally` closeFileHandles ((\(af, fn, h) -> h) <$> fileHndls)
            printFilePaths logPths 
            pure $ Right logPths
        )
                        
docRunRaw :: (forall m1 m a. TestPlan m1 m a FullDocEffects) -> DList Text
docRunRaw pln = extractDocLog $ testRun pln filters docExecution executeDocumentRaw runConfig

docRun :: (forall m1 m a. TestPlan m1 m a FullDocEffects) -> DList Text
docRun pln = extractDocLog $ testRun pln filters docExecution executeDocumentPretty runConfig

runInIO :: IO ()
runInIO = ioRun plan

runLogToFile :: IO ()
runLogToFile = do 
                ePths <- ioRunToFile NoConsole False plan executeInIO normalExecution 
                eitherf ePths 
                  (\err -> putStrLn $ "Error Encountered\n" <> txt err)
                  (\pths ->
                      let 
                        jsnItmsPth = find ((== jsonItemLogExt) . toS . fileExtension) pths
                      in
                        maybef jsnItmsPth
                          (putStrLn $ "Unable to generate report no: " <> jsonItemLogExt <> " file found in log files\n")
                          prepareFinalLogs
                  )

runConsoleAndFile :: IO ()
runConsoleAndFile = void $ ioRunToFile Console False plan executeInIO normalExecution 

docConsoleAndFile :: IO ()
docConsoleAndFile = void $ ioRunToFile Console True plan documentInIO docExecution

runInIORaw :: IO ()
runInIORaw = ioRunRaw plan

runNZInIO :: IO ()
runNZInIO = testRun plan filters normalExecution executeInIOConsoleRaw runConfig {country = NZ}

runDocument :: DList Text
runDocument = docRun plan

runDocumentToConsole :: IO ()
runDocumentToConsole = sequence_ . P.toList $ putStrLn <$> runDocument

plan :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
plan = validPlan doNothing doNothing doNothing doNothing

alwaysFailCheck :: PreRun effs
alwaysFailCheck = PreRun {
  runAction = pure (),
  checkHasRun = pure False
}

testRunFailHomeG2 :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testRunFailHomeG2 = validPlan doNothing -- rollOver0
                              doNothing -- goHome0
                              doNothing -- rollOver1
                              alwaysFailCheck -- goHome1

runFailHomeG2IO :: IO ()
runFailHomeG2IO = ioRun testRunFailHomeG2

runFailHomeG2Document :: DList Text
runFailHomeG2Document = docRun testRunFailHomeG2

testRunFailRolloverG1 :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testRunFailRolloverG1 = validPlan alwaysFailCheck doNothing doNothing doNothing

runFailRolloverG1Document :: DList Text
runFailRolloverG1Document = docRun testRunFailRolloverG1

runFailRolloverG1IO :: IO ()
runFailRolloverG1IO = ioRun testRunFailRolloverG1

dummyIOException :: Eff effs Bool
dummyIOException = (E.throw $ P.userError "Pretend IO Error") :: Eff effs Bool

exceptionInCheck :: PreRun effs
exceptionInCheck = PreRun {
  runAction = pure (),
  checkHasRun = dummyIOException
}

testRunFailExceptG2GoHomeCheck :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testRunFailExceptG2GoHomeCheck = validPlan doNothing doNothing doNothing exceptionInCheck

runExceptG2GoHomeCheckIO :: IO ()
runExceptG2GoHomeCheckIO = ioRun testRunFailExceptG2GoHomeCheck

exceptionInRollover :: PreRun effs
exceptionInRollover = PreRun {
  runAction = void dummyIOException,
  checkHasRun = pure True
}

testRunExceptG1Rollover:: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testRunExceptG1Rollover = validPlan exceptionInRollover doNothing doNothing doNothing

runExceptG1Rollover :: IO ()
runExceptG1Rollover = ioRun testRunExceptG1Rollover

justLogPreRun :: EFFLogger effs => PreTestStage -> PreRun effs
justLogPreRun stage = PreRun {
  runAction = log $ "Run Action: " <> txt stage,
  checkHasRun = log ("Check Action Run: " <> txt stage) $> True
}

testG1GoHomeLogging:: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testG1GoHomeLogging = validPlan (justLogPreRun Rollover) (justLogPreRun GoHome) doNothing doNothing

justLogPreRunFailCheck :: EFFLogger effs => PreTestStage -> PreRun effs
justLogPreRunFailCheck stage = PreRun {
  runAction = log $ "Run Action: " <> txt stage,
  checkHasRun = log ("Check Action Run: " <> txt stage) $> False
}

testG1GoHomeLoggingFailCheck :: forall m m1 effs a. EFFAllEffects effs => TestPlan m1 m a effs
testG1GoHomeLoggingFailCheck = validPlan (justLogPreRun Rollover) (justLogPreRunFailCheck GoHome) doNothing doNothing
