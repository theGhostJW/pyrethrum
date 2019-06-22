module LogTransformationTest where 

import Text.RawString.QQ
import           Pyrelude as P
import           Pyrelude.IO as PIO
import           Data.DList as DL
import Pyrelude.Test       as T
import AuxFiles
import Control.Monad
import LogTransformation
import LogTransformation.PrintLogDisplayElement
import PrettyPrintCommon
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified System.IO as S
import LogTransformation.Test as LT
import DSL.Logger
import DSL.LogProtocol as LP
import RunElementClasses
import Data.Aeson as A
import DSL.LogProtocol.PrettyPrint
import Prelude (String)
import LogTransformation.Stats
import LogTransformation.Common as LTC
import Data.Foldable as F
import qualified Data.Map.Strict as M
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

-- unit_demo_logs_prettyPrint :: IO ()
-- unit_demo_logs_prettyPrint = dumpFile (decodeUtf8 <$> testPrettyPrint rawFile) [relfile|steps.yaml|]

-- unit_demo_iteration :: IO ()
-- unit_demo_iteration = aggregateDumpFile testIterationStep rawFile [relfile|iterations.yaml|]

-- unit_demo_prettyPrint_iteration :: IO ()
-- unit_demo_prettyPrint_iteration = aggregateDumpFile testIterationPretyPrintStep rawFile [relfile|demoTemp.yaml|]

-- unit_demo_test_items :: IO ()
-- unit_demo_test_items = dumpFile (decodeUtf8 <$> testTestLogStep (testIterationStep rawFile)) [relfile|tests.jsoni|]

-- -- bug fix
-- unit_other_warnings_as_expected :: IO ()
-- unit_other_warnings_as_expected =
--   let
--     targetTest = "DemoProject.Test.Rough"
--     logSteps = testTestLogStepRaw (testIterationStep rawFile)
--     testDemoProjectTestRough = P.find (\case 
--                                         Test tr@(TestRecord titl address config status stats iterationsDesc) -> address == targetTest
--                                         _ -> False
--                                       ) logSteps

--     iteration100 :: TestLogElement -> Maybe IterationRecord
--     iteration100 = \case 
--                       Test tle -> P.find (\i -> iid (summary i) == ItemId (TestModule targetTest) 100) $ iterationsDesc tle
--                       _ -> Nothing

--     chkSingleInteractorWarning :: IterationRecord -> Assertion
--     chkSingleInteractorWarning i = 
--       let 
--         othWarnings = otherWarningsDesc i
--       in 
--         chkEq 1 $ P.length othWarnings
--   in 
--     maybef (testDemoProjectTestRough >>= iteration100)
--       (chkFail $ "iteration 100 of " <> targetTest <> " not found" )
--       chkSingleInteractorWarning

                            

-- unit_demo_test_items_pretty :: IO ()
-- unit_demo_test_items_pretty = dumpFile (decodeUtf8 <$> testTestLogPrettyPrintStep (testIterationStep rawFile)) [relfile|tests.yaml|]

-- ToDo - Pretty print test - may need to use state monad
-- ToDo - Test parser - totals etc
-- ToDo - Test parser - include errors - may need to restructure for errors  such as file missing ??
-- ToDo - plug into run
-- Move stats to top -- concatinate files 

_sampleStatsSimple = F.foldl' statsStep emptyStepAccum $ Right <$> sampleLog

sampleStats :: RunResults
sampleStats = 
  let 
    transParams = LogTransformParams {
      source = testSource,
      sink = const $ pure (),
      reducer = statsStepForReducer,
      itemDesrialiser = jsonDeserialiser,
      resultSerialiser = yamlSerialiser,    
      linNo = LineNo 1,
      accumulator = emptyStepAccum
    }
  in
    runResults (fst $ transformDList rawFile transParams)

_demo_pretty_print_LP = dumpFile (prettyPrintLogProtocol False <$> sampleLog) [relfile|raw.yaml|]

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

_base_results = dumpTxt (txtPretty $ iterationResults sampleStats) [relfile|baseResults.yaml|] 

_demo_test_stats = txtPretty $ testStatusCounts sampleStats

_demo_iteration_stats = txtPretty $ iterationStatusCounts sampleStats

unit_iteration_counts_correct = M.fromList [(Pass,8),(KnownError,2),(LTC.Warning,4),(Fail,10)] ... iterationStatusCounts sampleStats

unit_test_counts_correct = M.fromList [(Fail,4)] ... testStatusCounts sampleStats

unit_no_out_of_test_issues = M.empty ... LTC.outOfTest sampleStats

--TODO:: Out of test issues test correct

_demo_pretty_print_log = 
  let 
    transParams = LogTransformParams {
      source = testSource,
      sink = testSink,
      reducer = printLogDisplayStep sampleStats,
      itemDesrialiser = jsonDeserialiser,
      resultSerialiser = (toS . prettyPrintDisplayElement) :: PrintLogDisplayElement -> ByteString,    
      linNo = LineNo 1,
      accumulator = emptyIterationAccum
    }
  in
    dumpByteStrings (snd $ transformDList rawFile transParams) [relfile|pretty.yaml|] 

unit_demo_pretty_print = _demo_pretty_print_log 

sampleLog :: DList LogProtocol
sampleLog = fromRight' . A.eitherDecode . toS <$> rawFile

rawFile :: DList ByteString
rawFile = fromList . B.lines $ toS 
  [r|{"tag":"BoundaryLog","contents":{"tag":"StartRun","contents":[{"unRunTitle":"Sample RunConfig"},{"environment":"TST","country":"AU","runTitle":"Sample RunConfig","depth":"DeepRegression"}]}}
{"tag":"BoundaryLog","contents":{"tag":"FilterLog","contents":[{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough2"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough2"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple2"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple2"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null}]}}
{"tag":"BoundaryLog","contents":{"tag":"StartGroup","contents":{"unGroupTitle":"Group 1"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid x 10 is small","iid x 10 is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"unApStateDisplay":"ApState\n  { itemId = 100\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"unDStateDisplay":"V { iidx10 = 1000 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"result":{"tag":"GateFailExpected","contents":"this bug was introduced in an earlier version and will be fixed eventually"},"info":{"header":"iid x 10 is small","messageInfo":null}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100},{"result":{"tag":"Skip"},"info":{"header":"iid x 10 is big","messageInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":100}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"SHould Crash"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":""}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"Debug Stack"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"unApStateDisplay":"ApState\n  { itemId = 110\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110},{"tag":"AppEnsureError","contents":"I do not like 110 in prepstate"}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":110}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120},{"tag":"AppFileSystemError","contents":{"tag":"WriteFileError","contents":{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":120}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unApStateDisplay":"ApState\n  { itemId = 130\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130},{"unDStateDisplay":"V { iidx10 = 1300 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":130}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary THING THAT WILL BLOW UP"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140},{"tag":"AppIOError'","contents":["Exception raised when executing arbituary IO action with message: This is an arbitrary THING THAT WILL BLOW UP",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"C:\\Vids\\SystemDesign\\Blahhh.txt","ioe_errno":"Just 2"}]}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":140}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\Vid List.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150},{"unApStateDisplay":"ApState\n  { itemId = 150\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\Vid List.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150},{"unDStateDisplay":"V { iidx10 = 1500 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough"},"itmId":150}}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Rough"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid is small","iid is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"unApStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"unDStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"result":{"tag":"Pass"},"info":{"header":"iid is small","messageInfo":null}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100},{"result":{"tag":"Fail"},"info":{"header":"iid is big","messageInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":100}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110},{"unApStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110},{"unDStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":110}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":123,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123},{"tag":"AppEnsureError","contents":"Only even iids expected"}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":123}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130},{"unApStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130},{"unDStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":130}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140},{"unApStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140},{"unDStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":140}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150},{"unApStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150},{"unDStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple"},"itmId":150}}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Simple"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndGroup","contents":{"unGroupTitle":"Group 1"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartGroup","contents":{"unGroupTitle":"Group 2"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough2"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough2"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid x 10 is small","iid x 10 is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"unApStateDisplay":"ApState\n  { itemId = 100\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"unDStateDisplay":"V { iidx10 = 1000 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"result":{"tag":"GateFailExpected","contents":"this bug was introduced in an earlier version and will be fixed eventually"},"info":{"header":"iid x 10 is small","messageInfo":null}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100},{"result":{"tag":"Skip"},"info":{"header":"iid x 10 is big","messageInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":100}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"SHould Crash"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":""}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"Debug Stack"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"unApStateDisplay":"ApState\n  { itemId = 110\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110},{"tag":"AppEnsureError","contents":"I do not like 110 in prepstate"}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":110}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120},{"tag":"AppFileSystemError","contents":{"tag":"WriteFileError","contents":{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":120}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"unApStateDisplay":"ApState\n  { itemId = 130\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130},{"unDStateDisplay":"V { iidx10 = 1300 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":130}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary THING THAT WILL BLOW UP"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140},{"tag":"AppIOError'","contents":["Exception raised when executing arbituary IO action with message: This is an arbitrary THING THAT WILL BLOW UP",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"C:\\Vids\\SystemDesign\\Blahhh.txt","ioe_errno":"Just 2"}]}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":140}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\Vid List.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150},{"unApStateDisplay":"ApState\n  { itemId = 150\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\Vid List.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150},{"unDStateDisplay":"V { iidx10 = 1500 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Rough2"},"itmId":150}}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Rough2"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple2"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple2"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid is small","iid is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"unApStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"unDStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"result":{"tag":"Pass"},"info":{"header":"iid is small","messageInfo":null}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100},{"result":{"tag":"Fail"},"info":{"header":"iid is big","messageInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":100}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110},{"unApStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110},{"unDStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":110}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":123,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123},{"tag":"AppEnsureError","contents":"Only even iids expected"}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":123}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130},{"unApStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130},{"unDStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":130}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140},{"unApStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140},{"unDStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":140}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150},{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150},{"unApStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150},{"unDStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":{"tstModule":{"unTestModule":"DemoProject.Test.Simple2"},"itmId":150}}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Simple2"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndGroup","contents":{"unGroupTitle":"Group 2"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndRun"}}|]