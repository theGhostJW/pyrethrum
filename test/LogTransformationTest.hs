{-# LANGUAGE QuasiQuotes #-}

module LogTransformationTest where 

import Text.RawString.QQ
import           Pyrelude as P
import           Pyrelude.IO as PIO
import           Data.DList as DL
import Pyrelude.Test       as T
import AuxFiles
import LogTransformation
import LogTransformation.PrintLogDisplayElement as LTPDE
import Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import qualified System.IO as S
import DSL.LogProtocol as LP
import Data.Aeson as A
import DSL.LogProtocol.PrettyPrint
import LogTransformation.Stats
import LogTransformation.Common as LTC
import qualified Data.Map.Strict as M
import Control.Monad.Writer.Strict (WriterT)
import Control.Monad.State.Strict (State)
import DemoProject.Config

-- import LogTransformation.Iteration

runAgg :: (DList ByteString -> DList ByteString) -> DList ByteString -> DList Text
runAgg f l = decodeUtf8 <$> f l 

-- todo: get file utils really sorted
dumpFile ::  DList Text -> RelFile -> IO ()
dumpFile lst file = do
      ePth <- tempFile file
      eitherf ePth 
        throw
        (\pth -> do 
                  h <- S.openFile (toFilePath pth ) S.WriteMode 
                  sequence_ $ PIO.hPutStrLn h <$> lst
                  S.hClose h
                  S.print pth
        )

-- todo: get file utils really sorted
dumpTxt :: Text -> RelFile -> IO ()
dumpTxt txt' file = do 
                      ePth <- tempFile file
                      eitherf ePth
                        throw
                        (\p -> PIO.writeFile (toFilePath p) txt')

aggregateDumpFile :: (DList ByteString -> DList ByteString) -> DList ByteString -> RelFile -> IO ()
aggregateDumpFile func lst = dumpFile (runAgg func lst) 

dumpByteStrings :: DList ByteString -> RelFile -> IO ()
dumpByteStrings lst file = do
                            ePth <- tempFile file
                            eitherf ePth 
                              throw
                              (\pth -> do 
                                        h <- S.openFile (toFilePath pth ) S.WriteMode 
                                        sequence_ $ B.hPutStrLn h <$> lst
                                        S.hClose h
                                        S.print pth
                              )

display :: (DList ByteString -> DList ByteString) -> DList ByteString -> IO ()
display f l = sequence_ $ PIO.putStrLn <$> runAgg f l

_sampleRunResultsSimple :: StatsAccum
_sampleRunResultsSimple = F.foldl' statsStep emptyStatsAccum $ Right <$> sampleLog

type WriterStateByteString o a = WriterState ByteString o a
sampleRunResults :: RunResults
sampleRunResults = 
  let
    -- data LogTransformParams accum itm rsltItem m srcFmt snkFmt 
    -- type WriterState i o a = WriterT (DList o) (StateT (DList i) Identity) a
    transParams :: LogTransformParams StatsAccum LogProtocolOut StatsAccum (WriterT (DList o) (State (DList ByteString))) ByteString ByteString
    transParams = LogTransformParams {
      source = testSource,
      sink = const $ pure (),
      reducer = statsStepForReducer,
      itemDesrialiser = jsonDeserialiser,
      resultSerialiser = yamlSerialiser,    
      linNo = LineNo 1,
      accumulator = emptyStatsAccum
    }
  in
    runResults (fst $ transformDList rawFile transParams)
    -- can use for de bugging -- runResults (fst $ transformDList rawFileSmall transParams)

demo_pretty_print_LP = dumpFile (prettyPrintLogProtocol False <$> sampleLog) [relfile|raw.yaml|]

_demo_pretty_print_LP_with_reducer :: IO ()
_demo_pretty_print_LP_with_reducer = 
  let 
    transParams = LogTransformParams {
      source = testSource,
      sink = testSink,
      reducer = prettyPrintLogprotocolReducer,
      itemDesrialiser = jsonDeserialiser,
      resultSerialiser = id,    
      linNo = LineNo 1,
      accumulator = ()
    }
  in
    dumpFile (snd $ transformDList rawFile transParams) [relfile|raw.yaml|]

demo_test_stats :: Text
demo_test_stats = txtPretty $ testStatusCounts sampleRunResults

_base_results :: IO()
_base_results = dumpTxt (txtPretty $ iterationResults sampleRunResults) [relfile|baseResults.yaml|]

demo_iteration_stats = txtPretty $ iterationStatusCounts sampleRunResults

unit_iteration_counts_correct = 
  M.fromList [
              (Pass,         9),
              (KnownError,   2),
              (LTC.Warning,  2),
              (Fail,        20)
             ] ... iterationStatusCounts sampleRunResults

unit_test_counts_correct = M.fromList [(Pass,1), (Fail,4)] ... testStatusCounts sampleRunResults

unit_no_out_of_test_issues = M.empty ... LTC.outOfTest sampleRunResults

--TODO:: Out of test issues test correct

prettyPrintLog :: [PrintLogDisplayElement]
prettyPrintLog = 
  let 
    transParams = LogTransformParams {
      source = testSource,
      sink = testSink,
      reducer = printLogDisplayStep sampleRunResults,
      itemDesrialiser = jsonDeserialiser :: LineNo -> ByteString -> Either DeserialisationError LogProtocolOut,
      resultSerialiser = id,    
      linNo = LineNo 1,
      accumulator = emptyIterationAccum
    }
  in
    DL.toList . snd $ transformDList rawFile transParams 

prettyProblemsPrintLog :: [PrintLogDisplayElement]
prettyProblemsPrintLog =
  let 
    transParams = LogTransformParams {
      source = testSource,
      sink = testSink,
      reducer = printProblemsDisplayStep sampleRunResults,
      itemDesrialiser = jsonDeserialiser :: LineNo -> ByteString -> Either DeserialisationError LogProtocolOut,
      resultSerialiser = id,    
      linNo = LineNo 1,
      accumulator = emptyProbleIterationAccum
    }
  in
    DL.toList . snd $ transformDList rawFile transParams

isPassingTestHeader :: PrintLogDisplayElement -> Bool
isPassingTestHeader = 
  \case 
    LTPDE.StartTest{..} -> status == Pass
    _ -> False

unit_problems_no_passing_tests = 
  0 ... P.count isPassingTestHeader (prettyProblemsPrintLog :: [PrintLogDisplayElement])

unit_unfilterd_has_passing_tests :: IO()  
unit_unfilterd_has_passing_tests = 
  1 ... P.count isPassingTestHeader (prettyPrintLog :: [PrintLogDisplayElement])

isPassingIterationHeader :: PrintLogDisplayElement -> Bool
isPassingIterationHeader = 
  \case 
    LTPDE.Iteration LTPDE.IterationRecord{..} -> executionStatus outcome == Pass
    _ -> False
     
unit_problems_no_passing_iterations :: IO()
unit_problems_no_passing_iterations = 
  0 ... P.count isPassingIterationHeader (prettyProblemsPrintLog :: [PrintLogDisplayElement])

unit_unfilterd_has_passing_iterations :: IO()  
unit_unfilterd_has_passing_iterations = 
  9 ... P.count isPassingIterationHeader (prettyPrintLog :: [PrintLogDisplayElement])


_demo_pretty_print_log :: IO ()
_demo_pretty_print_log = 
  let 
    transParams = LogTransformParams {
      source = testSource,
      sink = testSink,
      reducer = printLogDisplayStep sampleRunResults,
      itemDesrialiser = jsonDeserialiser,
      resultSerialiser = toS . prettyPrintDisplayElement, -- :: PrintLogDisplayElement e -> ByteString,    
      linNo = LineNo 1,
      accumulator = emptyIterationAccum
    }
  in
    dumpByteStrings (snd $ transformDList rawFile transParams) [relfile|pretty.yaml|] 

_demo_pretty_print_problems_log :: IO ()
_demo_pretty_print_problems_log = 
  let 
    transParams = LogTransformParams {
      source = testSource,
      sink = testSink,
      reducer = printProblemsDisplayStep sampleRunResults,
      itemDesrialiser = jsonDeserialiser,
      resultSerialiser = toS . prettyPrintDisplayElement, 
      linNo = LineNo 1,
      accumulator = emptyProbleIterationAccum
    }
  in
    dumpByteStrings (snd $ transformDList rawFile transParams) [relfile|pretty.yaml|] 

unit_demo_pretty_print = _demo_pretty_print_log 

sampleLog :: DList LogProtocolOut
sampleLog = fromRight' . A.eitherDecode . toS <$> rawFile

-- source doc can be generated by running:
-- >> stack repl --test
-- >> runToFileAndConsole
-- >> copy from jsoni log file created  
rawFile :: DList ByteString
rawFile = fromList . B.lines $ toS 
  [r|{"tag":"BoundaryLog","contents":{"tag":"StartRun","contents":[{"unRunTitle":"Sample RunConfig"},{"environment":"TST","country":"AU","runTitle":"Sample RunConfig","depth":"DeepRegression"}]}}
{"tag":"BoundaryLog","contents":{"tag":"FilterLog","contents":[{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.RoughDisabled"},"testTitle":"This is a Rough Disabled Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.RoughDisabled"},"countries":["AU","NZ"],"active":false,"header":"This is a Rough Disabled Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":"test must be is active"},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.RoughIntState"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.RoughIntState"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough2"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough2"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple2"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple2"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null}]}}
{"tag":"BoundaryLog","contents":{"tag":"StartGroup","contents":{"unGroupTitle":"Group 1"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid x 10 is small","iid x 10 is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"unApStateJSON":{"itemId":100,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"unDStateJSON":{"iidx10":1000}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"result":{"tag":"GateFailExpected","contents":"this bug was introduced in an earlier version and will be fixed eventually"},"info":{"message":"iid x 10 is small","additionalInfo":"the iid x 10 (1000) is expected to be less than 200"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"result":{"tag":"Skip"},"info":{"message":"iid x 10 is big","additionalInfo":"the iid x 10 (1000) is expected to be greater than 500"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"SHould Crash"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":""}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"Debug Stack"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"unApStateJSON":{"itemId":110,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"tag":"EnsureError","contents":"I do not like 110 in prepstate"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Error","contents":{"tag":"EnsureError","contents":"I do not like 110 in prepstate"}}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"tag":"FileSystemError","contents":["WriteFileError",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}]}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unApStateJSON":{"itemId":130,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unDStateJSON":{"iidx10":1300}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary THING THAT WILL BLOW UP"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140},{"tag":"IOError'","contents":["Exception raised when executing arbitrary IO action with message: This is an arbitrary THING THAT WILL BLOW UP",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"C:\\Vids\\SystemDesign\\Blahhh.txt","ioe_errno":"Just 2"}]}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\Vid List.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150},{"unApStateJSON":{"itemId":150,"filePath":"C:\\Vids\\SystemDesign\\Vid List.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150},{"unDStateJSON":{"iidx10":1500}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":160},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":160,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":160},{"unApStateJSON":{"itemId":160,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":160},{"unDStateJSON":{"iidx10":1600}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":160},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":160}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":170},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":170,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":170},{"unApStateJSON":{"itemId":170,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":170},{"unDStateJSON":{"iidx10":1700}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":170},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":170}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":180},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":180,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":180},{"unApStateJSON":{"itemId":180,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":180},{"unDStateJSON":{"iidx10":1800}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":180},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":180}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":190},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":190,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":190},{"unApStateJSON":{"itemId":190,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":190},{"unDStateJSON":{"iidx10":1900}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":190},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":190}}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Rough"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid is small","iid is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"unApStateJSON":{"simpleMessage":"Success","itemId":100}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"unDStateJSON":{"simpleMessage":"Success","itemId":100}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"result":{"tag":"Pass"},"info":{"message":"iid is small","additionalInfo":null}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"result":{"tag":"Fail"},"info":{"message":"iid is big","additionalInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110},{"unApStateJSON":{"simpleMessage":"Success","itemId":110}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110},{"unDStateJSON":{"simpleMessage":"Success","itemId":110}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":123,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123},{"tag":"EnsureError","contents":"Only even iids expected"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130},{"unApStateJSON":{"simpleMessage":"Success","itemId":130}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130},{"unDStateJSON":{"simpleMessage":"Success","itemId":130}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140},{"unApStateJSON":{"simpleMessage":"Success","itemId":140}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140},{"unDStateJSON":{"simpleMessage":"Success","itemId":140}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150},{"unApStateJSON":{"simpleMessage":"Success","itemId":150}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150},{"unDStateJSON":{"simpleMessage":"Success","itemId":150}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150}}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Simple"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.RoughIntState"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.RoughIntState"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.RoughIntState"},"itmId":110},{"unWhenClause":"Whene Statement"},{"unThenClause":"Then Statement"},{"iid":110,"pre":"Whene Statement","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Then Statement"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.RoughIntState"},"itmId":110},{"unApStateJSON":5}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.RoughIntState"},"itmId":110},{"unDStateJSON":6}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.RoughIntState"},"itmId":110},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.RoughIntState"},"itmId":110}}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.RoughIntState"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndGroup","contents":{"unGroupTitle":"Group 1"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartGroup","contents":{"unGroupTitle":"Group 2"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough2"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough2"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid x 10 is small","iid x 10 is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"unApStateJSON":{"itemId":100,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"unDStateJSON":{"iidx10":1000}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"result":{"tag":"GateFailExpected","contents":"this bug was introduced in an earlier version and will be fixed eventually"},"info":{"message":"iid x 10 is small","additionalInfo":"the iid x 10 (1000) is expected to be less than 200"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"result":{"tag":"Skip"},"info":{"message":"iid x 10 is big","additionalInfo":"the iid x 10 (1000) is expected to be greater than 500"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"SHould Crash"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":""}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"Debug Stack"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"unApStateJSON":{"itemId":110,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"tag":"EnsureError","contents":"I do not like 110 in prepstate"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Error","contents":{"tag":"EnsureError","contents":"I do not like 110 in prepstate"}}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120},{"tag":"FileSystemError","contents":["WriteFileError",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}]}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"unApStateJSON":{"itemId":130,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"unDStateJSON":{"iidx10":1300}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary THING THAT WILL BLOW UP"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140},{"tag":"IOError'","contents":["Exception raised when executing arbitrary IO action with message: This is an arbitrary THING THAT WILL BLOW UP",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"C:\\Vids\\SystemDesign\\Blahhh.txt","ioe_errno":"Just 2"}]}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\Vid List.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150},{"unApStateJSON":{"itemId":150,"filePath":"C:\\Vids\\SystemDesign\\Vid List.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150},{"unDStateJSON":{"iidx10":1500}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":160},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":160,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":160},{"unApStateJSON":{"itemId":160,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":160},{"unDStateJSON":{"iidx10":1600}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":160},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":160}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":170},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":170,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":170},{"unApStateJSON":{"itemId":170,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":170},{"unDStateJSON":{"iidx10":1700}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":170},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":170}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":180},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":180,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":180},{"unApStateJSON":{"itemId":180,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":180},{"unDStateJSON":{"iidx10":1800}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":180},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":180}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":190},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":190,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":190},{"unApStateJSON":{"itemId":190,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":190},{"unDStateJSON":{"iidx10":1900}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":190},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":190}}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Rough2"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple2"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple2"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid is small","iid is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"unApStateJSON":{"simpleMessage":"Success","itemId":100}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"unDStateJSON":{"simpleMessage":"Success","itemId":100}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"result":{"tag":"Pass"},"info":{"message":"iid is small","additionalInfo":null}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"result":{"tag":"Fail"},"info":{"message":"iid is big","additionalInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110},{"unApStateJSON":{"simpleMessage":"Success","itemId":110}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110},{"unDStateJSON":{"simpleMessage":"Success","itemId":110}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":123,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123},{"tag":"EnsureError","contents":"Only even iids expected"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130},{"unApStateJSON":{"simpleMessage":"Success","itemId":130}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130},{"unDStateJSON":{"simpleMessage":"Success","itemId":130}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140},{"unApStateJSON":{"simpleMessage":"Success","itemId":140}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140},{"unDStateJSON":{"simpleMessage":"Success","itemId":140}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150},{"unApStateJSON":{"simpleMessage":"Success","itemId":150}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150},{"unDStateJSON":{"simpleMessage":"Success","itemId":150}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150}}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Simple2"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndGroup","contents":{"unGroupTitle":"Group 2"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndRun"}}|]

sampleLogSmall :: DList LogProtocolOut
sampleLogSmall = fromRight' . A.eitherDecode . toS <$> rawFileSmall

-- a fragment of the above can be used for de bugging after updating fragment
rawFileSmall :: DList ByteString
rawFileSmall = fromList . B.lines $ toS 
  [r|{"tag":"BoundaryLog","contents":{"tag":"StartRun","contents":[{"unRunTitle":"Sample RunConfig"},{"environment":"TST","country":"AU","runTitle":"Sample RunConfig","depth":"DeepRegression"}]}}
{"tag":"BoundaryLog","contents":{"tag":"FilterLog","contents":[{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.RoughDisabled"},"testTitle":"This is a Rough Disabled Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.RoughDisabled"},"countries":["AU","NZ"],"active":false,"header":"This is a Rough Disabled Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":"test must be is active"},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.RoughIntState"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.RoughIntState"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough2"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough2"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple2"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple2"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null}]}}
{"tag":"BoundaryLog","contents":{"tag":"StartGroup","contents":{"unGroupTitle":"Group 1"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"tag":"FileSystemError","contents":["WriteFileError",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}]}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unApStateJSON":{"itemId":130,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unDStateJSON":{"iidx10":1300}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130}}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Roug"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndGroup","contents":{"unGroupTitle":"Group 1"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndRun"}}|]
