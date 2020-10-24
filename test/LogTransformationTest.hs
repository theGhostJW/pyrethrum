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

-- import LogTransformation.Iteration

runAgg :: (DList ByteString -> DList ByteString) -> DList ByteString -> DList Text
runAgg f l = decodeUtf8 <$> f l 

-- todo: get file utils really sorted
dumpFile ::  IO AbsDir -> DList Text -> RelFile -> IO ()
dumpFile projRoot lst file = do
      ePth <- tempFile projRoot file
      eitherf ePth 
        throw
        (\pth -> do 
                  h <- S.openFile (toFilePath pth ) S.WriteMode 
                  sequence_ $ PIO.hPutStrLn h <$> lst
                  S.hClose h
                  S.print pth
        )

aggregateDumpFile :: IO AbsDir -> (DList ByteString -> DList ByteString) -> DList ByteString -> RelFile -> IO ()
aggregateDumpFile projRoot func lst = dumpFile projRoot (runAgg func lst) 

dumpByteStrings :: IO AbsDir -> DList ByteString -> RelFile -> IO ()
dumpByteStrings projRoot lst file = do
                            ePth <- tempFile projRoot file
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

_thisExeDir :: IO AbsDir
_thisExeDir = parent <$> (parseAbsFile =<< getExecutablePath)

demo_pretty_print_LP = dumpFile _thisExeDir (prettyPrintLogProtocol False . logInfo <$> sampleLog) [relfile|raw.yaml|]

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
    dumpFile _thisExeDir (snd $ transformDList rawFile transParams) [relfile|raw.yaml|]

demo_test_stats :: Text
demo_test_stats = txtPretty $ testStatusCounts sampleRunResults

_base_results :: IO()
_base_results = dumpTxt _thisExeDir (txtPretty $ iterationResults sampleRunResults) [relfile|baseResults.yaml|]

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
    dumpByteStrings _thisExeDir (snd $ transformDList rawFile transParams) [relfile|pretty.yaml|] 

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
    dumpByteStrings _thisExeDir (snd $ transformDList rawFile transParams) [relfile|pretty.yaml|] 

unit_demo_pretty_print = _demo_pretty_print_log 

sampleLog :: DList LogProtocolOut
sampleLog = fromRight' . A.eitherDecode . toS <$> rawFile

-- source doc can be generated by running:
-- >> stack repl --test
-- >> runToFileAndConsole
-- >> copy from jsoni log file created  
rawFile :: DList ByteString
rawFile = fromList . B.lines $ toS 
  [r|{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605879000000,"idx":{"unLogIndex":1}},"time":1595363605879000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartRun","runTitle":{"unRunTitle":"Sample RunConfig"},"runUtcOffsetMins":600,"runConfig":{"environment":"TST","country":"AU","runTitle":"Sample RunConfig","depth":"DeepRegression"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605879000000,"idx":{"unLogIndex":2}},"time":1595363605879000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"FilterLog","contents":[{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.RoughDisabled"},"testTitle":"This is a Rough Disabled Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.RoughDisabled"},"countries":["AU","NZ"],"active":false,"header":"This is a Rough Disabled Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":"test must be is active"},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.RoughIntState"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.RoughIntState"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough2"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough2"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple2"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple2"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605879000000,"idx":{"unLogIndex":3}},"time":1595363605879000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartGroup","contents":{"unGroupTitle":"Group 1"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605895000000,"idx":{"unLogIndex":4}},"time":1595363605895000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605895000000,"idx":{"unLogIndex":5}},"time":1595363605895000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid x 10 is small","iid x 10 is big"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605895000000,"idx":{"unLogIndex":6}},"time":1595363605895000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605895000000,"idx":{"unLogIndex":7}},"time":1595363605895000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605895000000,"idx":{"unLogIndex":8}},"time":1595363605895000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605895000000,"idx":{"unLogIndex":9}},"time":1595363605895000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605904000000,"idx":{"unLogIndex":10}},"time":1595363605904000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605904000000,"idx":{"unLogIndex":11}},"time":1595363605904000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"unApStateJSON":{"itemId":100,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605905000000,"idx":{"unLogIndex":12}},"time":1595363605905000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605905000000,"idx":{"unLogIndex":13}},"time":1595363605905000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"unDStateJSON":{"iidx10":1000}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605906000000,"idx":{"unLogIndex":14}},"time":1595363605906000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605906000000,"idx":{"unLogIndex":15}},"time":1595363605906000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"result":{"tag":"GateFailExpected","contents":"this bug was introduced in an earlier version and will be fixed eventually"},"info":{"message":"iid x 10 is small","additionalInfo":"the iid x 10 (1000) is expected to be less than 200"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605906000000,"idx":{"unLogIndex":16}},"time":1595363605906000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"result":{"tag":"Skip"},"info":{"message":"iid x 10 is big","additionalInfo":"the iid x 10 (1000) is expected to be greater than 500"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605906000000,"idx":{"unLogIndex":17}},"time":1595363605906000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605906000000,"idx":{"unLogIndex":18}},"time":1595363605906000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605906000000,"idx":{"unLogIndex":19}},"time":1595363605906000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605906000000,"idx":{"unLogIndex":20}},"time":1595363605906000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605908000000,"idx":{"unLogIndex":21}},"time":1595363605908000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605908000000,"idx":{"unLogIndex":22}},"time":1595363605908000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605912000000,"idx":{"unLogIndex":23}},"time":1595363605912000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"SHould Crash"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605912000000,"idx":{"unLogIndex":24}},"time":1595363605912000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":""}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605912000000,"idx":{"unLogIndex":25}},"time":1595363605912000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"Debug Stack"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605914000000,"idx":{"unLogIndex":26}},"time":1595363605914000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605914000000,"idx":{"unLogIndex":27}},"time":1595363605914000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"unApStateJSON":{"itemId":110,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605915000000,"idx":{"unLogIndex":28}},"time":1595363605915000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605915000000,"idx":{"unLogIndex":29}},"time":1595363605915000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"tag":"EnsureError","contents":"I do not like 110 in prepstate"}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605915000000,"idx":{"unLogIndex":30}},"time":1595363605915000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605915000000,"idx":{"unLogIndex":31}},"time":1595363605915000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605915000000,"idx":{"unLogIndex":19}},"time":1595363605915000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Error","contents":{"tag":"EnsureError","contents":"I do not like 110 in prepstate"}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605916000000,"idx":{"unLogIndex":20}},"time":1595363605916000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605916000000,"idx":{"unLogIndex":21}},"time":1595363605916000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605916000000,"idx":{"unLogIndex":22}},"time":1595363605916000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605916000000,"idx":{"unLogIndex":23}},"time":1595363605916000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605916000000,"idx":{"unLogIndex":24}},"time":1595363605916000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"tag":"FileSystemError","contents":["WriteFileError",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}]}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605916000000,"idx":{"unLogIndex":25}},"time":1595363605916000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605916000000,"idx":{"unLogIndex":26}},"time":1595363605916000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605917000000,"idx":{"unLogIndex":27}},"time":1595363605917000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605917000000,"idx":{"unLogIndex":28}},"time":1595363605917000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605917000000,"idx":{"unLogIndex":29}},"time":1595363605917000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605917000000,"idx":{"unLogIndex":30}},"time":1595363605917000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605917000000,"idx":{"unLogIndex":31}},"time":1595363605917000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605919000000,"idx":{"unLogIndex":32}},"time":1595363605919000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605919000000,"idx":{"unLogIndex":33}},"time":1595363605919000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605942000000,"idx":{"unLogIndex":34}},"time":1595363605942000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605942000000,"idx":{"unLogIndex":35}},"time":1595363605942000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605943000000,"idx":{"unLogIndex":36}},"time":1595363605943000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605943000000,"idx":{"unLogIndex":37}},"time":1595363605943000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605943000000,"idx":{"unLogIndex":38}},"time":1595363605943000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605943000000,"idx":{"unLogIndex":39}},"time":1595363605943000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unApStateJSON":{"itemId":130,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605944000000,"idx":{"unLogIndex":40}},"time":1595363605944000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605944000000,"idx":{"unLogIndex":41}},"time":1595363605944000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unDStateJSON":{"iidx10":1300}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605944000000,"idx":{"unLogIndex":42}},"time":1595363605944000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605944000000,"idx":{"unLogIndex":43}},"time":1595363605944000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605944000000,"idx":{"unLogIndex":44}},"time":1595363605944000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605944000000,"idx":{"unLogIndex":45}},"time":1595363605944000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605945000000,"idx":{"unLogIndex":46}},"time":1595363605945000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605945000000,"idx":{"unLogIndex":47}},"time":1595363605945000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605946000000,"idx":{"unLogIndex":48}},"time":1595363605946000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605946000000,"idx":{"unLogIndex":49}},"time":1595363605946000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605962000000,"idx":{"unLogIndex":50}},"time":1595363605962000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary THING THAT WILL BLOW UP"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605963000000,"idx":{"unLogIndex":48}},"time":1595363605963000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140},{"tag":"IOError'","contents":["Exception raised when executing arbitrary IO action with message: This is an arbitrary THING THAT WILL BLOW UP",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"C:\\Vids\\SystemDesign\\Blahhh.txt","ioe_errno":"Just 2"}]}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605963000000,"idx":{"unLogIndex":49}},"time":1595363605963000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605964000000,"idx":{"unLogIndex":50}},"time":1595363605964000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605964000000,"idx":{"unLogIndex":51}},"time":1595363605964000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605964000000,"idx":{"unLogIndex":52}},"time":1595363605964000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605964000000,"idx":{"unLogIndex":53}},"time":1595363605964000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\Vid List.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605965000000,"idx":{"unLogIndex":54}},"time":1595363605965000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605965000000,"idx":{"unLogIndex":55}},"time":1595363605965000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605967000000,"idx":{"unLogIndex":56}},"time":1595363605967000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605968000000,"idx":{"unLogIndex":57}},"time":1595363605968000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605985000000,"idx":{"unLogIndex":58}},"time":1595363605985000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605985000000,"idx":{"unLogIndex":59}},"time":1595363605985000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150},{"unApStateJSON":{"itemId":150,"filePath":"C:\\Vids\\SystemDesign\\Vid List.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605986000000,"idx":{"unLogIndex":60}},"time":1595363605986000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605986000000,"idx":{"unLogIndex":61}},"time":1595363605986000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150},{"unDStateJSON":{"iidx10":1500}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605986000000,"idx":{"unLogIndex":62}},"time":1595363605986000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605986000000,"idx":{"unLogIndex":63}},"time":1595363605986000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605986000000,"idx":{"unLogIndex":64}},"time":1595363605986000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":160},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":160,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605986000000,"idx":{"unLogIndex":65}},"time":1595363605986000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605986000000,"idx":{"unLogIndex":66}},"time":1595363605986000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605988000000,"idx":{"unLogIndex":67}},"time":1595363605988000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605988000000,"idx":{"unLogIndex":68}},"time":1595363605988000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605992000000,"idx":{"unLogIndex":69}},"time":1595363605992000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605992000000,"idx":{"unLogIndex":70}},"time":1595363605992000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":160},{"unApStateJSON":{"itemId":160,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605992000000,"idx":{"unLogIndex":71}},"time":1595363605992000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605993000000,"idx":{"unLogIndex":72}},"time":1595363605993000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":160},{"unDStateJSON":{"iidx10":1600}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605993000000,"idx":{"unLogIndex":73}},"time":1595363605993000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605993000000,"idx":{"unLogIndex":74}},"time":1595363605993000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":160},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605993000000,"idx":{"unLogIndex":75}},"time":1595363605993000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":160}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605993000000,"idx":{"unLogIndex":76}},"time":1595363605993000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":170},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":170,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605993000000,"idx":{"unLogIndex":77}},"time":1595363605993000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605993000000,"idx":{"unLogIndex":78}},"time":1595363605993000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605995000000,"idx":{"unLogIndex":79}},"time":1595363605995000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605995000000,"idx":{"unLogIndex":80}},"time":1595363605995000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605997000000,"idx":{"unLogIndex":81}},"time":1595363605997000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605997000000,"idx":{"unLogIndex":82}},"time":1595363605997000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":170},{"unApStateJSON":{"itemId":170,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605998000000,"idx":{"unLogIndex":83}},"time":1595363605998000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605998000000,"idx":{"unLogIndex":84}},"time":1595363605998000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":170},{"unDStateJSON":{"iidx10":1700}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605998000000,"idx":{"unLogIndex":85}},"time":1595363605998000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605999000000,"idx":{"unLogIndex":86}},"time":1595363605999000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":170},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605999000000,"idx":{"unLogIndex":87}},"time":1595363605999000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":170}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605999000000,"idx":{"unLogIndex":88}},"time":1595363605999000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":180},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":180,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605999000000,"idx":{"unLogIndex":89}},"time":1595363605999000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363605999000000,"idx":{"unLogIndex":90}},"time":1595363605999000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606001000000,"idx":{"unLogIndex":91}},"time":1595363606001000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606002000000,"idx":{"unLogIndex":92}},"time":1595363606002000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606015000000,"idx":{"unLogIndex":93}},"time":1595363606015000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606015000000,"idx":{"unLogIndex":94}},"time":1595363606015000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":180},{"unApStateJSON":{"itemId":180,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606015000000,"idx":{"unLogIndex":95}},"time":1595363606015000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606015000000,"idx":{"unLogIndex":96}},"time":1595363606015000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":180},{"unDStateJSON":{"iidx10":1800}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606015000000,"idx":{"unLogIndex":97}},"time":1595363606015000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606015000000,"idx":{"unLogIndex":98}},"time":1595363606015000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":180},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606016000000,"idx":{"unLogIndex":99}},"time":1595363606016000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":180}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606016000000,"idx":{"unLogIndex":100}},"time":1595363606016000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":190},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":190,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606016000000,"idx":{"unLogIndex":101}},"time":1595363606016000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606016000000,"idx":{"unLogIndex":102}},"time":1595363606016000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606018000000,"idx":{"unLogIndex":103}},"time":1595363606018000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606018000000,"idx":{"unLogIndex":104}},"time":1595363606018000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606031000000,"idx":{"unLogIndex":105}},"time":1595363606031000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606031000000,"idx":{"unLogIndex":106}},"time":1595363606031000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":190},{"unApStateJSON":{"itemId":190,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606032000000,"idx":{"unLogIndex":107}},"time":1595363606032000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606032000000,"idx":{"unLogIndex":108}},"time":1595363606032000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":190},{"unDStateJSON":{"iidx10":1900}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606032000000,"idx":{"unLogIndex":109}},"time":1595363606032000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606036000000,"idx":{"unLogIndex":110}},"time":1595363606036000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":190},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606036000000,"idx":{"unLogIndex":111}},"time":1595363606036000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":190}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606036000000,"idx":{"unLogIndex":112}},"time":1595363606036000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Rough"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606036000000,"idx":{"unLogIndex":113}},"time":1595363606036000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606037000000,"idx":{"unLogIndex":114}},"time":1595363606037000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid is small","iid is big"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606037000000,"idx":{"unLogIndex":115}},"time":1595363606037000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606037000000,"idx":{"unLogIndex":116}},"time":1595363606037000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606037000000,"idx":{"unLogIndex":117}},"time":1595363606037000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606037000000,"idx":{"unLogIndex":118}},"time":1595363606037000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"unApStateJSON":{"simpleMessage":"Success","itemId":100}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606037000000,"idx":{"unLogIndex":119}},"time":1595363606037000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606037000000,"idx":{"unLogIndex":120}},"time":1595363606037000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"unDStateJSON":{"simpleMessage":"Success","itemId":100}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606038000000,"idx":{"unLogIndex":121}},"time":1595363606038000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606038000000,"idx":{"unLogIndex":122}},"time":1595363606038000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"result":{"tag":"Pass"},"info":{"message":"iid is small","additionalInfo":null}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606038000000,"idx":{"unLogIndex":123}},"time":1595363606038000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"result":{"tag":"Fail"},"info":{"message":"iid is big","additionalInfo":null}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606038000000,"idx":{"unLogIndex":124}},"time":1595363606038000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606038000000,"idx":{"unLogIndex":125}},"time":1595363606038000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606038000000,"idx":{"unLogIndex":126}},"time":1595363606038000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606038000000,"idx":{"unLogIndex":127}},"time":1595363606038000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606038000000,"idx":{"unLogIndex":128}},"time":1595363606038000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606039000000,"idx":{"unLogIndex":129}},"time":1595363606039000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110},{"unApStateJSON":{"simpleMessage":"Success","itemId":110}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606039000000,"idx":{"unLogIndex":130}},"time":1595363606039000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606039000000,"idx":{"unLogIndex":131}},"time":1595363606039000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110},{"unDStateJSON":{"simpleMessage":"Success","itemId":110}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606039000000,"idx":{"unLogIndex":132}},"time":1595363606039000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606039000000,"idx":{"unLogIndex":133}},"time":1595363606039000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606039000000,"idx":{"unLogIndex":134}},"time":1595363606039000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":123,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606039000000,"idx":{"unLogIndex":135}},"time":1595363606039000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606039000000,"idx":{"unLogIndex":136}},"time":1595363606039000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606040000000,"idx":{"unLogIndex":137}},"time":1595363606040000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123},{"tag":"EnsureError","contents":"Only even iids expected"}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606040000000,"idx":{"unLogIndex":138}},"time":1595363606040000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606040000000,"idx":{"unLogIndex":139}},"time":1595363606040000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606040000000,"idx":{"unLogIndex":140}},"time":1595363606040000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606040000000,"idx":{"unLogIndex":141}},"time":1595363606040000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606040000000,"idx":{"unLogIndex":142}},"time":1595363606040000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606040000000,"idx":{"unLogIndex":143}},"time":1595363606040000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606040000000,"idx":{"unLogIndex":144}},"time":1595363606040000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606041000000,"idx":{"unLogIndex":145}},"time":1595363606041000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130},{"unApStateJSON":{"simpleMessage":"Success","itemId":130}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606041000000,"idx":{"unLogIndex":146}},"time":1595363606041000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606041000000,"idx":{"unLogIndex":147}},"time":1595363606041000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130},{"unDStateJSON":{"simpleMessage":"Success","itemId":130}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606041000000,"idx":{"unLogIndex":148}},"time":1595363606041000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606041000000,"idx":{"unLogIndex":149}},"time":1595363606041000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606041000000,"idx":{"unLogIndex":150}},"time":1595363606041000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606041000000,"idx":{"unLogIndex":151}},"time":1595363606041000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606042000000,"idx":{"unLogIndex":152}},"time":1595363606042000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606042000000,"idx":{"unLogIndex":153}},"time":1595363606042000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606042000000,"idx":{"unLogIndex":154}},"time":1595363606042000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140},{"unApStateJSON":{"simpleMessage":"Success","itemId":140}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606042000000,"idx":{"unLogIndex":155}},"time":1595363606042000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606042000000,"idx":{"unLogIndex":156}},"time":1595363606042000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140},{"unDStateJSON":{"simpleMessage":"Success","itemId":140}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606042000000,"idx":{"unLogIndex":157}},"time":1595363606042000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606042000000,"idx":{"unLogIndex":158}},"time":1595363606042000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606043000000,"idx":{"unLogIndex":159}},"time":1595363606043000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606043000000,"idx":{"unLogIndex":160}},"time":1595363606043000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606043000000,"idx":{"unLogIndex":161}},"time":1595363606043000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606043000000,"idx":{"unLogIndex":162}},"time":1595363606043000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606044000000,"idx":{"unLogIndex":163}},"time":1595363606044000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150},{"unApStateJSON":{"simpleMessage":"Success","itemId":150}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606044000000,"idx":{"unLogIndex":164}},"time":1595363606044000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606044000000,"idx":{"unLogIndex":165}},"time":1595363606044000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150},{"unDStateJSON":{"simpleMessage":"Success","itemId":150}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606045000000,"idx":{"unLogIndex":166}},"time":1595363606045000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606045000000,"idx":{"unLogIndex":167}},"time":1595363606045000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606045000000,"idx":{"unLogIndex":168}},"time":1595363606045000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Simple"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606045000000,"idx":{"unLogIndex":169}},"time":1595363606045000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.RoughIntState"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.RoughIntState"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606045000000,"idx":{"unLogIndex":170}},"time":1595363606045000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.RoughIntState"},"itmId":110},{"unWhenClause":"Whene Statement"},{"unThenClause":"Then Statement"},{"iid":110,"pre":"Whene Statement","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Then Statement"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606045000000,"idx":{"unLogIndex":171}},"time":1595363606045000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606046000000,"idx":{"unLogIndex":172}},"time":1595363606046000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606046000000,"idx":{"unLogIndex":173}},"time":1595363606046000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606046000000,"idx":{"unLogIndex":174}},"time":1595363606046000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.RoughIntState"},"itmId":110},{"unApStateJSON":5}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606046000000,"idx":{"unLogIndex":175}},"time":1595363606046000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606046000000,"idx":{"unLogIndex":176}},"time":1595363606046000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.RoughIntState"},"itmId":110},{"unDStateJSON":6}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606046000000,"idx":{"unLogIndex":177}},"time":1595363606046000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606046000000,"idx":{"unLogIndex":178}},"time":1595363606046000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.RoughIntState"},"itmId":110},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":null}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606046000000,"idx":{"unLogIndex":179}},"time":1595363606046000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.RoughIntState"},"itmId":110}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606046000000,"idx":{"unLogIndex":180}},"time":1595363606046000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.RoughIntState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606047000000,"idx":{"unLogIndex":181}},"time":1595363606047000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndGroup","contents":{"unGroupTitle":"Group 1"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606047000000,"idx":{"unLogIndex":182}},"time":1595363606047000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartGroup","contents":{"unGroupTitle":"Group 2"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606047000000,"idx":{"unLogIndex":183}},"time":1595363606047000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough2"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough2"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606048000000,"idx":{"unLogIndex":184}},"time":1595363606048000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid x 10 is small","iid x 10 is big"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606048000000,"idx":{"unLogIndex":185}},"time":1595363606048000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606048000000,"idx":{"unLogIndex":186}},"time":1595363606048000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606050000000,"idx":{"unLogIndex":187}},"time":1595363606050000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606050000000,"idx":{"unLogIndex":188}},"time":1595363606050000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606063000000,"idx":{"unLogIndex":189}},"time":1595363606063000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606063000000,"idx":{"unLogIndex":190}},"time":1595363606063000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"unApStateJSON":{"itemId":100,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606063000000,"idx":{"unLogIndex":191}},"time":1595363606063000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606064000000,"idx":{"unLogIndex":192}},"time":1595363606064000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"unDStateJSON":{"iidx10":1000}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606064000000,"idx":{"unLogIndex":193}},"time":1595363606064000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606064000000,"idx":{"unLogIndex":194}},"time":1595363606064000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"result":{"tag":"GateFailExpected","contents":"this bug was introduced in an earlier version and will be fixed eventually"},"info":{"message":"iid x 10 is small","additionalInfo":"the iid x 10 (1000) is expected to be less than 200"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606064000000,"idx":{"unLogIndex":195}},"time":1595363606064000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"result":{"tag":"Skip"},"info":{"message":"iid x 10 is big","additionalInfo":"the iid x 10 (1000) is expected to be greater than 500"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606064000000,"idx":{"unLogIndex":196}},"time":1595363606064000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606065000000,"idx":{"unLogIndex":197}},"time":1595363606065000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606065000000,"idx":{"unLogIndex":198}},"time":1595363606065000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606065000000,"idx":{"unLogIndex":199}},"time":1595363606065000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606067000000,"idx":{"unLogIndex":200}},"time":1595363606067000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606068000000,"idx":{"unLogIndex":201}},"time":1595363606068000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606072000000,"idx":{"unLogIndex":202}},"time":1595363606072000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"SHould Crash"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606073000000,"idx":{"unLogIndex":203}},"time":1595363606073000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":""}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606073000000,"idx":{"unLogIndex":204}},"time":1595363606073000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"Debug Stack"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606074000000,"idx":{"unLogIndex":205}},"time":1595363606074000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606074000000,"idx":{"unLogIndex":206}},"time":1595363606074000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"unApStateJSON":{"itemId":110,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606074000000,"idx":{"unLogIndex":207}},"time":1595363606074000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606075000000,"idx":{"unLogIndex":208}},"time":1595363606075000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"tag":"EnsureError","contents":"I do not like 110 in prepstate"}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606075000000,"idx":{"unLogIndex":209}},"time":1595363606075000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606075000000,"idx":{"unLogIndex":210}},"time":1595363606075000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606075000000,"idx":{"unLogIndex":198}},"time":1595363606075000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Error","contents":{"tag":"EnsureError","contents":"I do not like 110 in prepstate"}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606075000000,"idx":{"unLogIndex":199}},"time":1595363606075000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606075000000,"idx":{"unLogIndex":200}},"time":1595363606075000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606075000000,"idx":{"unLogIndex":201}},"time":1595363606075000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606076000000,"idx":{"unLogIndex":202}},"time":1595363606076000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606076000000,"idx":{"unLogIndex":203}},"time":1595363606076000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120},{"tag":"FileSystemError","contents":["WriteFileError",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}]}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606076000000,"idx":{"unLogIndex":204}},"time":1595363606076000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606076000000,"idx":{"unLogIndex":205}},"time":1595363606076000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606076000000,"idx":{"unLogIndex":206}},"time":1595363606076000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606076000000,"idx":{"unLogIndex":207}},"time":1595363606076000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606077000000,"idx":{"unLogIndex":208}},"time":1595363606077000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606077000000,"idx":{"unLogIndex":209}},"time":1595363606077000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606077000000,"idx":{"unLogIndex":210}},"time":1595363606077000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606078000000,"idx":{"unLogIndex":211}},"time":1595363606078000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606078000000,"idx":{"unLogIndex":212}},"time":1595363606078000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606081000000,"idx":{"unLogIndex":213}},"time":1595363606081000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606081000000,"idx":{"unLogIndex":214}},"time":1595363606081000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606081000000,"idx":{"unLogIndex":215}},"time":1595363606081000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606081000000,"idx":{"unLogIndex":216}},"time":1595363606081000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606081000000,"idx":{"unLogIndex":217}},"time":1595363606081000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606082000000,"idx":{"unLogIndex":218}},"time":1595363606082000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"unApStateJSON":{"itemId":130,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606082000000,"idx":{"unLogIndex":219}},"time":1595363606082000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606082000000,"idx":{"unLogIndex":220}},"time":1595363606082000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"unDStateJSON":{"iidx10":1300}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606082000000,"idx":{"unLogIndex":221}},"time":1595363606082000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606083000000,"idx":{"unLogIndex":222}},"time":1595363606083000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606083000000,"idx":{"unLogIndex":223}},"time":1595363606083000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606083000000,"idx":{"unLogIndex":224}},"time":1595363606083000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606083000000,"idx":{"unLogIndex":225}},"time":1595363606083000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606083000000,"idx":{"unLogIndex":226}},"time":1595363606083000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606084000000,"idx":{"unLogIndex":227}},"time":1595363606084000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606085000000,"idx":{"unLogIndex":228}},"time":1595363606085000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606093000000,"idx":{"unLogIndex":229}},"time":1595363606093000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary THING THAT WILL BLOW UP"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606094000000,"idx":{"unLogIndex":227}},"time":1595363606094000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140},{"tag":"IOError'","contents":["Exception raised when executing arbitrary IO action with message: This is an arbitrary THING THAT WILL BLOW UP",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"C:\\Vids\\SystemDesign\\Blahhh.txt","ioe_errno":"Just 2"}]}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606094000000,"idx":{"unLogIndex":228}},"time":1595363606094000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606094000000,"idx":{"unLogIndex":229}},"time":1595363606094000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606095000000,"idx":{"unLogIndex":230}},"time":1595363606095000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140},{"result":{"tag":"Skip"},"info":{"message":"pass every time","additionalInfo":"Validation checks not executed"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606095000000,"idx":{"unLogIndex":231}},"time":1595363606095000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606095000000,"idx":{"unLogIndex":232}},"time":1595363606095000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\Vid List.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606095000000,"idx":{"unLogIndex":233}},"time":1595363606095000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606095000000,"idx":{"unLogIndex":234}},"time":1595363606095000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606097000000,"idx":{"unLogIndex":235}},"time":1595363606097000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606097000000,"idx":{"unLogIndex":236}},"time":1595363606097000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606111000000,"idx":{"unLogIndex":237}},"time":1595363606111000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606112000000,"idx":{"unLogIndex":238}},"time":1595363606112000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150},{"unApStateJSON":{"itemId":150,"filePath":"C:\\Vids\\SystemDesign\\Vid List.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606112000000,"idx":{"unLogIndex":239}},"time":1595363606112000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606113000000,"idx":{"unLogIndex":240}},"time":1595363606113000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150},{"unDStateJSON":{"iidx10":1500}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606113000000,"idx":{"unLogIndex":241}},"time":1595363606113000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606113000000,"idx":{"unLogIndex":242}},"time":1595363606113000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606113000000,"idx":{"unLogIndex":243}},"time":1595363606113000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":160},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":160,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606113000000,"idx":{"unLogIndex":244}},"time":1595363606113000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606113000000,"idx":{"unLogIndex":245}},"time":1595363606113000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606115000000,"idx":{"unLogIndex":246}},"time":1595363606115000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606116000000,"idx":{"unLogIndex":247}},"time":1595363606116000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606127000000,"idx":{"unLogIndex":248}},"time":1595363606127000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606127000000,"idx":{"unLogIndex":249}},"time":1595363606127000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":160},{"unApStateJSON":{"itemId":160,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606128000000,"idx":{"unLogIndex":250}},"time":1595363606128000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606128000000,"idx":{"unLogIndex":251}},"time":1595363606128000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":160},{"unDStateJSON":{"iidx10":1600}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606128000000,"idx":{"unLogIndex":252}},"time":1595363606128000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606128000000,"idx":{"unLogIndex":253}},"time":1595363606128000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":160},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606128000000,"idx":{"unLogIndex":254}},"time":1595363606128000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":160}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606129000000,"idx":{"unLogIndex":255}},"time":1595363606129000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":170},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":170,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606129000000,"idx":{"unLogIndex":256}},"time":1595363606129000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606129000000,"idx":{"unLogIndex":257}},"time":1595363606129000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606130000000,"idx":{"unLogIndex":258}},"time":1595363606130000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606131000000,"idx":{"unLogIndex":259}},"time":1595363606131000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606143000000,"idx":{"unLogIndex":260}},"time":1595363606143000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606144000000,"idx":{"unLogIndex":261}},"time":1595363606144000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":170},{"unApStateJSON":{"itemId":170,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606144000000,"idx":{"unLogIndex":262}},"time":1595363606144000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606145000000,"idx":{"unLogIndex":263}},"time":1595363606145000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":170},{"unDStateJSON":{"iidx10":1700}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606145000000,"idx":{"unLogIndex":264}},"time":1595363606145000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606146000000,"idx":{"unLogIndex":265}},"time":1595363606146000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":170},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606146000000,"idx":{"unLogIndex":266}},"time":1595363606146000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":170}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606146000000,"idx":{"unLogIndex":267}},"time":1595363606146000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":180},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":180,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606146000000,"idx":{"unLogIndex":268}},"time":1595363606146000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606146000000,"idx":{"unLogIndex":269}},"time":1595363606146000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606148000000,"idx":{"unLogIndex":270}},"time":1595363606148000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606148000000,"idx":{"unLogIndex":271}},"time":1595363606148000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606162000000,"idx":{"unLogIndex":272}},"time":1595363606162000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606162000000,"idx":{"unLogIndex":273}},"time":1595363606162000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":180},{"unApStateJSON":{"itemId":180,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606163000000,"idx":{"unLogIndex":274}},"time":1595363606163000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606163000000,"idx":{"unLogIndex":275}},"time":1595363606163000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":180},{"unDStateJSON":{"iidx10":1800}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606163000000,"idx":{"unLogIndex":276}},"time":1595363606163000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606163000000,"idx":{"unLogIndex":277}},"time":1595363606163000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":180},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606163000000,"idx":{"unLogIndex":278}},"time":1595363606163000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":180}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606163000000,"idx":{"unLogIndex":279}},"time":1595363606163000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":190},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":190,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["pass every time"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606163000000,"idx":{"unLogIndex":280}},"time":1595363606163000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606164000000,"idx":{"unLogIndex":281}},"time":1595363606164000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606166000000,"idx":{"unLogIndex":282}},"time":1595363606166000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606166000000,"idx":{"unLogIndex":283}},"time":1595363606166000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606170000000,"idx":{"unLogIndex":284}},"time":1595363606170000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606170000000,"idx":{"unLogIndex":285}},"time":1595363606170000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":190},{"unApStateJSON":{"itemId":190,"filePath":"C:\\Vids\\SystemDesign\\VidList.txt","fileText":"Pre ~ Post !!","exePath":"NOT IMPLEMENTED"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606170000000,"idx":{"unLogIndex":286}},"time":1595363606170000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606170000000,"idx":{"unLogIndex":287}},"time":1595363606170000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":190},{"unDStateJSON":{"iidx10":1900}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606170000000,"idx":{"unLogIndex":288}},"time":1595363606170000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606171000000,"idx":{"unLogIndex":289}},"time":1595363606171000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":190},{"result":{"tag":"Pass"},"info":{"message":"pass every time","additionalInfo":"this is additoinal info \nblahh\nblahh\nblahh"}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606171000000,"idx":{"unLogIndex":290}},"time":1595363606171000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":190}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606171000000,"idx":{"unLogIndex":291}},"time":1595363606171000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Rough2"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606171000000,"idx":{"unLogIndex":292}},"time":1595363606171000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple2"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple2"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606171000000,"idx":{"unLogIndex":293}},"time":1595363606171000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid is small","iid is big"],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606171000000,"idx":{"unLogIndex":294}},"time":1595363606171000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606172000000,"idx":{"unLogIndex":295}},"time":1595363606172000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606172000000,"idx":{"unLogIndex":296}},"time":1595363606172000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606172000000,"idx":{"unLogIndex":297}},"time":1595363606172000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"unApStateJSON":{"simpleMessage":"Success","itemId":100}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606172000000,"idx":{"unLogIndex":298}},"time":1595363606172000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606172000000,"idx":{"unLogIndex":299}},"time":1595363606172000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"unDStateJSON":{"simpleMessage":"Success","itemId":100}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606172000000,"idx":{"unLogIndex":300}},"time":1595363606172000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606172000000,"idx":{"unLogIndex":301}},"time":1595363606172000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"result":{"tag":"Pass"},"info":{"message":"iid is small","additionalInfo":null}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606173000000,"idx":{"unLogIndex":302}},"time":1595363606173000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"result":{"tag":"Fail"},"info":{"message":"iid is big","additionalInfo":null}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606173000000,"idx":{"unLogIndex":303}},"time":1595363606173000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606173000000,"idx":{"unLogIndex":304}},"time":1595363606173000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606173000000,"idx":{"unLogIndex":305}},"time":1595363606173000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606173000000,"idx":{"unLogIndex":306}},"time":1595363606173000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606173000000,"idx":{"unLogIndex":307}},"time":1595363606173000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606173000000,"idx":{"unLogIndex":308}},"time":1595363606173000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110},{"unApStateJSON":{"simpleMessage":"Success","itemId":110}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606173000000,"idx":{"unLogIndex":309}},"time":1595363606173000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606173000000,"idx":{"unLogIndex":310}},"time":1595363606173000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110},{"unDStateJSON":{"simpleMessage":"Success","itemId":110}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606174000000,"idx":{"unLogIndex":311}},"time":1595363606174000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606174000000,"idx":{"unLogIndex":312}},"time":1595363606174000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606174000000,"idx":{"unLogIndex":313}},"time":1595363606174000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":123,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606174000000,"idx":{"unLogIndex":314}},"time":1595363606174000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606174000000,"idx":{"unLogIndex":315}},"time":1595363606174000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606174000000,"idx":{"unLogIndex":316}},"time":1595363606174000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123},{"tag":"EnsureError","contents":"Only even iids expected"}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606174000000,"idx":{"unLogIndex":317}},"time":1595363606174000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSkipped","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123}}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606174000000,"idx":{"unLogIndex":318}},"time":1595363606174000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606174000000,"idx":{"unLogIndex":319}},"time":1595363606174000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606175000000,"idx":{"unLogIndex":320}},"time":1595363606175000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606175000000,"idx":{"unLogIndex":321}},"time":1595363606175000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606175000000,"idx":{"unLogIndex":322}},"time":1595363606175000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606175000000,"idx":{"unLogIndex":323}},"time":1595363606175000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606175000000,"idx":{"unLogIndex":324}},"time":1595363606175000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130},{"unApStateJSON":{"simpleMessage":"Success","itemId":130}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606175000000,"idx":{"unLogIndex":325}},"time":1595363606175000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606175000000,"idx":{"unLogIndex":326}},"time":1595363606175000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130},{"unDStateJSON":{"simpleMessage":"Success","itemId":130}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606176000000,"idx":{"unLogIndex":327}},"time":1595363606176000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606176000000,"idx":{"unLogIndex":328}},"time":1595363606176000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606176000000,"idx":{"unLogIndex":329}},"time":1595363606176000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606176000000,"idx":{"unLogIndex":330}},"time":1595363606176000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606176000000,"idx":{"unLogIndex":331}},"time":1595363606176000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606176000000,"idx":{"unLogIndex":332}},"time":1595363606176000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606176000000,"idx":{"unLogIndex":333}},"time":1595363606176000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140},{"unApStateJSON":{"simpleMessage":"Success","itemId":140}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606176000000,"idx":{"unLogIndex":334}},"time":1595363606176000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606177000000,"idx":{"unLogIndex":335}},"time":1595363606177000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140},{"unDStateJSON":{"simpleMessage":"Success","itemId":140}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606177000000,"idx":{"unLogIndex":336}},"time":1595363606177000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606177000000,"idx":{"unLogIndex":337}},"time":1595363606177000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606177000000,"idx":{"unLogIndex":338}},"time":1595363606177000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606177000000,"idx":{"unLogIndex":339}},"time":1595363606177000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606177000000,"idx":{"unLogIndex":340}},"time":1595363606177000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact start"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606177000000,"idx":{"unLogIndex":341}},"time":1595363606177000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"interact end"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606177000000,"idx":{"unLogIndex":342}},"time":1595363606177000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150},{"unApStateJSON":{"simpleMessage":"Success","itemId":150}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606177000000,"idx":{"unLogIndex":343}},"time":1595363606177000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606178000000,"idx":{"unLogIndex":344}},"time":1595363606178000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150},{"unDStateJSON":{"simpleMessage":"Success","itemId":150}}]}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606178000000,"idx":{"unLogIndex":345}},"time":1595363606178000000,"logInfo":{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606178000000,"idx":{"unLogIndex":346}},"time":1595363606178000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606178000000,"idx":{"unLogIndex":347}},"time":1595363606178000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Simple2"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606178000000,"idx":{"unLogIndex":348}},"time":1595363606178000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndGroup","contents":{"unGroupTitle":"Group 2"}}}}
{"logIndex":{"rnId":"local","threadIdx":1,"time":1595363606178000000,"idx":{"unLogIndex":349}},"time":1595363606178000000,"logInfo":{"tag":"BoundaryLog","contents":{"tag":"EndRun"}}}|]

sampleLogSmall :: DList LogProtocolOut
sampleLogSmall = fromRight' . A.eitherDecode . toS <$> rawFileSmall

-- a fragment of the above can be used for de bugging after updating fragment - add a subset of the above :q
-- if and when you need it
rawFileSmall :: DList ByteString
rawFileSmall = fromList . B.lines $ toS 
  [r||]
