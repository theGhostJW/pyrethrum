module LogTransformationTest where 

import Text.RawString.QQ
import           Pyrelude as P
import           Pyrelude.IO as PIO
import           Data.DList
import Pyrelude.Test       as T
import AuxFiles
import Control.Monad
import LogTransformation
import PrettyPrintCommon
import Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import qualified System.IO as S
import LogTransformation.Test as LT
import DSL.Logger

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

aggregateDumpFile :: (DList ByteString -> DList ByteString) -> DList ByteString -> RelFile -> IO ()
aggregateDumpFile func lst = dumpFile (runAgg func lst) 

dumpFileSimple ::  DList ByteString -> RelFile -> IO ()
dumpFileSimple = uu

display :: (DList ByteString -> DList ByteString) -> DList ByteString -> IO ()
display f l = sequence_ $ PIO.putStrLn <$> runAgg f l

unit_demo_prettyPrint :: IO ()
unit_demo_prettyPrint = display testPrettyPrint rawFile

unit_demo_iteration :: IO ()
unit_demo_iteration = aggregateDumpFile testIterationStep rawFile [relfile|iterations.yaml|]

unit_demo_prettyPrint_iteration :: IO ()
unit_demo_prettyPrint_iteration = aggregateDumpFile testIterationPretyPrintStep rawFile [relfile|demoTemp.yaml|]

unit_demo_test_items_pretty :: IO ()
unit_demo_test_items_pretty = dumpFile (decodeUtf8 <$> testTestLogPrettyPrintStep (testIterationStep rawFile)) [relfile|tests.yaml|]

unit_demo_test_items :: IO ()
unit_demo_test_items = dumpFile (decodeUtf8 <$> testTestLogStep (testIterationStep rawFile)) [relfile|tests.jsoni|]

-- ToDo - Pretty print test
-- ToDo - Test parser - totals etc
-- ToDo - Test parser - include errors - may need to restructure for errors  such as file missing ??
-- ToDo - plug into run
-- Move stats to top -- concatinate files

rawFile :: DList ByteString
rawFile = fromList . B.lines $ toS 
  [r|{"tag":"BoundaryLog","contents":{"tag":"StartRun","contents":[{"unRunTitle":"Sample RunConfig"},{"environment":"TST","country":"AU","runTitle":"Sample RunConfig","depth":"DeepRegression"}]}}
{"tag":"BoundaryLog","contents":{"tag":"FilterLog","contents":[{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough2"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough2"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple2"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple2"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null}]}}
{"tag":"BoundaryLog","contents":{"tag":"StartGroup","contents":{"unGroupTitle":"Group 1"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough"},100],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid x 10 is small","iid x 10 is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough"},100],{"unApStateDisplay":"ApState\n  { itemId = 100\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough"},100],{"unDStateDisplay":"V { iidx10 = 1000 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[[{"unTestModule":"DemoProject.Test.Rough"},100],{"result":{"tag":"GateFailExpected","contents":"this bug was introduced in an earlier version and will be fixed eventually"},"info":{"header":"iid x 10 is small","messageInfo":null}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[[{"unTestModule":"DemoProject.Test.Rough"},100],{"result":{"tag":"Skip"},"info":{"header":"iid x 10 is big","messageInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough"},100]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough"},110],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"SHould Crash"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":""}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"Debug Stack"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough"},110],{"unApStateDisplay":"ApState\n  { itemId = 110\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateFailure","contents":[[{"unTestModule":"DemoProject.Test.Rough"},110],{"tag":"AppEnsureError","contents":"I do not like 110 in prepstate"}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough"},110]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough"},120],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[[{"unTestModule":"DemoProject.Test.Rough"},120],{"tag":"AppFileSystemError","contents":{"tag":"WriteFileError","contents":{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough"},120]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough"},130],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough"},130],{"unApStateDisplay":"ApState\n  { itemId = 130\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough"},130],{"unDStateDisplay":"V { iidx10 = 1300 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough"},130]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough"},140],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary THING THAT WILL BLOW UP"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[[{"unTestModule":"DemoProject.Test.Rough"},140],{"tag":"AppIOError'","contents":["Exception raised when executing arbituary IO action with message: This is an arbitrary THING THAT WILL BLOW UP",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"C:\\Vids\\SystemDesign\\Blahhh.txt","ioe_errno":"Just 2"}]}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough"},140]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough"},150],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\Vid List.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough"},150],{"unApStateDisplay":"ApState\n  { itemId = 150\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\Vid List.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough"},150],{"unDStateDisplay":"V { iidx10 = 1500 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough"},150]}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Rough"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple"},100],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid is small","iid is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple"},100],{"unApStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple"},100],{"unDStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[[{"unTestModule":"DemoProject.Test.Simple"},100],{"result":{"tag":"Pass"},"info":{"header":"iid is small","messageInfo":null}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[[{"unTestModule":"DemoProject.Test.Simple"},100],{"result":{"tag":"Fail"},"info":{"header":"iid is big","messageInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple"},100]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple"},110],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple"},110],{"unApStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple"},110],{"unDStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple"},110]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple"},123],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":123,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[[{"unTestModule":"DemoProject.Test.Simple"},123],{"tag":"AppEnsureError","contents":"Only even iids expected"}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple"},123]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple"},130],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple"},130],{"unApStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple"},130],{"unDStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple"},130]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple"},140],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple"},140],{"unApStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple"},140],{"unDStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple"},140]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple"},150],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple"},150],{"unApStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple"},150],{"unDStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple"},150]}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Simple"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndGroup","contents":{"unGroupTitle":"Group 1"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartGroup","contents":{"unGroupTitle":"Group 2"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Rough2"},"testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Rough2"},"countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},100],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid x 10 is small","iid x 10 is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},100],{"unApStateDisplay":"ApState\n  { itemId = 100\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},100],{"unDStateDisplay":"V { iidx10 = 1000 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},100],{"result":{"tag":"GateFailExpected","contents":"this bug was introduced in an earlier version and will be fixed eventually"},"info":{"header":"iid x 10 is small","messageInfo":null}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},100],{"result":{"tag":"Skip"},"info":{"header":"iid x 10 is big","messageInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough2"},100]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},110],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"SHould Crash"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":""}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"Debug Stack"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},110],{"unApStateDisplay":"ApState\n  { itemId = 110\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateFailure","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},110],{"tag":"AppEnsureError","contents":"I do not like 110 in prepstate"}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough2"},110]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},120],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},120],{"tag":"AppFileSystemError","contents":{"tag":"WriteFileError","contents":{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough2"},120]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},130],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},130],{"unApStateDisplay":"ApState\n  { itemId = 130\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},130],{"unDStateDisplay":"V { iidx10 = 1300 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough2"},130]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},140],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary THING THAT WILL BLOW UP"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},140],{"tag":"AppIOError'","contents":["Exception raised when executing arbituary IO action with message: This is an arbitrary THING THAT WILL BLOW UP",{"ioe_description":"No such file or directory","ioe_location":"openFile","ioe_type":"NoSuchThing","ioe_filename":"C:\\Vids\\SystemDesign\\Blahhh.txt","ioe_errno":"Just 2"}]}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough2"},140]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},150],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\Vid List.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Message","contents":"Hi"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"Warning","contents":"a warning"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"IOAction","contents":"This is an arbitrary Put Line"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},150],{"unApStateDisplay":"ApState\n  { itemId = 150\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\Vid List.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = \"Pre ~ Post !!\"\n  }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Rough2"},150],{"unDStateDisplay":"V { iidx10 = 1500 }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Rough2"},150]}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Rough2"}}}
{"tag":"BoundaryLog","contents":{"tag":"StartTest","contents":{"testModAddress":{"unTestModule":"DemoProject.Test.Simple2"},"testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":{"unTestModule":"DemoProject.Test.Simple2"},"countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}}}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},100],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid is small","iid is big"],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},100],{"unApStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},100],{"unDStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},100],{"result":{"tag":"Pass"},"info":{"header":"iid is small","messageInfo":null}}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"CheckOutcome","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},100],{"result":{"tag":"Fail"},"info":{"header":"iid is big","messageInfo":null}}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple2"},100]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},110],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},110],{"unApStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},110],{"unDStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple2"},110]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},123],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":123,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorFailure","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},123],{"tag":"AppEnsureError","contents":"Only even iids expected"}]}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple2"},123]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},130],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},130],{"unApStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},130],{"unDStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple2"},130]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},140],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},140],{"unApStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},140],{"unDStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple2"},140]}}
{"tag":"BoundaryLog","contents":{"tag":"StartIteration","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},150],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartInteraction"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"InteractorSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},150],{"unApStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartPrepState"}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"PrepStateSuccess","contents":[[{"unTestModule":"DemoProject.Test.Simple2"},150],{"unDStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}}}
{"tag":"IterationLog","contents":{"tag":"Run","contents":{"tag":"StartChecks"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndIteration","contents":[{"unTestModule":"DemoProject.Test.Simple2"},150]}}
{"tag":"BoundaryLog","contents":{"tag":"EndTest","contents":{"unTestModule":"DemoProject.Test.Simple2"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndGroup","contents":{"unGroupTitle":"Group 2"}}}
{"tag":"BoundaryLog","contents":{"tag":"EndRun"}}|]