module LogTransformationTest where 

import           Pyrelude as P
import           Data.DList
import Pyrelude.Test       as T
import AuxFiles
import Control.Monad
import LogTransformation
import Text.RawString.QQ
import Data.ByteString.Char8 as B
import qualified Data.Foldable as F

unit_demo :: IO ()
unit_demo = eitherf (testPrettyPrint rawFile)
              (\e -> P.putStr $ "ERROR !!!" <> txt e)
              (\bsList -> 
                let 
                  slList :: DList (IO ()) 
                  slList = P.putStrLn . decodeUtf8 <$> bsList 
                in 
                  sequence_ slList
                )

rawFile :: DList ByteString
rawFile = fromList . B.lines $ toS 
  [r|{"tag":"StartRun","contents":[{"unRunTitle":"Sample RunConfig"},{"environment":"TST","country":"AU","runTitle":"Sample RunConfig","depth":"DeepRegression"}]}
  {"tag":"FilterLog","contents":[{"testInfo":{"testModAddress":"DemoProject.Test.Rough","testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":"DemoProject.Test.Rough","countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":"DemoProject.Test.Simple","testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":"DemoProject.Test.Simple","countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":"DemoProject.Test.Rough2","testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":"DemoProject.Test.Rough2","countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null},{"testInfo":{"testModAddress":"DemoProject.Test.Simple2","testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":"DemoProject.Test.Simple2","countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}},"reasonForRejection":null}]}
  {"tag":"StartGroup","contents":{"unGroupTitle":"Group 1"}}
  {"tag":"StartTest","contents":{"testModAddress":"DemoProject.Test.Rough","testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":"DemoProject.Test.Rough","countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough",100],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid x 10 is small","iid x 10 is big"],"post":"Post"}]}
  {"tag":"Message","contents":"Hi"}
  {"tag":"Warning","contents":"a warning"}
  {"tag":"IOAction","contents":"This is an arbitrary Put Line"}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Rough",100],{"unApStateDisplay":"ApState\n  { itemId = 100\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = Right \"Pre ~ Post !!\"\n  }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Rough",100],{"unDStateDisplay":"V { iidx10 = 1000 }"}]}
  {"tag":"CheckOutcome","contents":[["DemoProject.Test.Rough",100],{"result":{"tag":"GateFail"},"info":{"header":"iid x 10 is small","messageInfo":null}}]}
  {"tag":"CheckOutcome","contents":[["DemoProject.Test.Rough",100],{"result":{"tag":"Skip"},"info":{"header":"iid x 10 is big","messageInfo":null}}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough",100]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough",110],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"Message","contents":"Hi"}
  {"tag":"Warning","contents":"a warning"}
  {"tag":"IOAction","contents":"This is an arbitrary Put Line"}
  {"tag":"Message","contents":"SHould Crash"}
  {"tag":"Message","contents":""}
  {"tag":"IOAction","contents":"Debug Stack"}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Rough",110],{"unApStateDisplay":"ApState\n  { itemId = 110\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = Right \"Pre ~ Post !!\"\n  }"}]}
  {"tag":"PrepStateFailure","contents":[["DemoProject.Test.Rough",110],{"tag":"AppEnsureError","contents":"I do not like 110 in prepstate"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough",110]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough",120],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorFailure","contents":[["DemoProject.Test.Rough",120],{"tag":"AppFileSystemError","contents":{"tag":"WriteFileError","contents":{"ioe_description":"No such file or directory","ioe_location":"openBinaryFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}}}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough",120]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough",130],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"Message","contents":"Hi"}
  {"tag":"Warning","contents":"a warning"}
  {"tag":"IOAction","contents":"This is an arbitrary Put Line"}
  {"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}
  {"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}
  {"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Rough",130],{"unApStateDisplay":"ApState\n  { itemId = 130\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = Right \"Pre ~ Post !!\"\n  }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Rough",130],{"unDStateDisplay":"V { iidx10 = 1300 }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough",130]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough",140],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"Message","contents":"Hi"}
  {"tag":"Warning","contents":"a warning"}
  {"tag":"IOAction","contents":"This is an arbitrary Put Line"}
  {"tag":"IOAction","contents":"This is an arbitrary THING THAT WILL BLOW UP"}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Rough",140],{"unApStateDisplay":"ApState\n  { itemId = 140\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = Right \"Pre ~ Post !!\"\n  }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Rough",140],{"unDStateDisplay":"V { iidx10 = 1400 }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough",140]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough",150],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\Vid List.txt","checks":[],"post":"Post"}]}
  {"tag":"Message","contents":"Hi"}
  {"tag":"Warning","contents":"a warning"}
  {"tag":"IOAction","contents":"This is an arbitrary Put Line"}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Rough",150],{"unApStateDisplay":"ApState\n  { itemId = 150\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\Vid List.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = Right \"Pre ~ Post !!\"\n  }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Rough",150],{"unDStateDisplay":"V { iidx10 = 1500 }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough",150]}
  {"tag":"EndTest","contents":"DemoProject.Test.Rough"}
  {"tag":"StartTest","contents":{"testModAddress":"DemoProject.Test.Simple","testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":"DemoProject.Test.Simple","countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}}}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple",100],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid is small","iid is big"],"post":"Post"}]}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Simple",100],{"unApStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Simple",100],{"unDStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}
  {"tag":"CheckOutcome","contents":[["DemoProject.Test.Simple",100],{"result":{"tag":"Pass"},"info":{"header":"iid is small","messageInfo":null}}]}
  {"tag":"CheckOutcome","contents":[["DemoProject.Test.Simple",100],{"result":{"tag":"Fail"},"info":{"header":"iid is big","messageInfo":null}}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple",100]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple",110],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Simple",110],{"unApStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Simple",110],{"unDStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple",110]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple",123],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":123,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorFailure","contents":[["DemoProject.Test.Simple",123],{"tag":"AppEnsureError","contents":"Only even iids expected"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple",123]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple",130],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Simple",130],{"unApStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Simple",130],{"unDStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple",130]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple",140],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Simple",140],{"unApStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Simple",140],{"unDStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple",140]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple",150],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Simple",150],{"unApStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Simple",150],{"unDStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple",150]}
  {"tag":"EndTest","contents":"DemoProject.Test.Simple"}
  {"tag":"EndGroup","contents":{"unGroupTitle":"Group 1"}}
  {"tag":"StartGroup","contents":{"unGroupTitle":"Group 2"}}
  {"tag":"StartTest","contents":{"testModAddress":"DemoProject.Test.Rough2","testTitle":"This is a Rough Test","testConfig":{"minDepth":"DeepRegression","address":"DemoProject.Test.Rough2","countries":["AU","NZ"],"active":true,"header":"This is a Rough Test","environments":["TST","UAT","PreProd"]}}}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough2",100],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid x 10 is small","iid x 10 is big"],"post":"Post"}]}
  {"tag":"Message","contents":"Hi"}
  {"tag":"Warning","contents":"a warning"}
  {"tag":"IOAction","contents":"This is an arbitrary Put Line"}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Rough2",100],{"unApStateDisplay":"ApState\n  { itemId = 100\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = Right \"Pre ~ Post !!\"\n  }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Rough2",100],{"unDStateDisplay":"V { iidx10 = 1000 }"}]}
  {"tag":"CheckOutcome","contents":[["DemoProject.Test.Rough2",100],{"result":{"tag":"GateFail"},"info":{"header":"iid x 10 is small","messageInfo":null}}]}
  {"tag":"CheckOutcome","contents":[["DemoProject.Test.Rough2",100],{"result":{"tag":"Skip"},"info":{"header":"iid x 10 is big","messageInfo":null}}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough2",100]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough2",110],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"Message","contents":"Hi"}
  {"tag":"Warning","contents":"a warning"}
  {"tag":"IOAction","contents":"This is an arbitrary Put Line"}
  {"tag":"Message","contents":"SHould Crash"}
  {"tag":"Message","contents":""}
  {"tag":"IOAction","contents":"Debug Stack"}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Rough2",110],{"unApStateDisplay":"ApState\n  { itemId = 110\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = Right \"Pre ~ Post !!\"\n  }"}]}
  {"tag":"PrepStateFailure","contents":[["DemoProject.Test.Rough2",110],{"tag":"AppEnsureError","contents":"I do not like 110 in prepstate"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough2",110]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough2",120],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":120,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorFailure","contents":[["DemoProject.Test.Rough2",120],{"tag":"AppFileSystemError","contents":{"tag":"WriteFileError","contents":{"ioe_description":"No such file or directory","ioe_location":"openBinaryFile","ioe_type":"NoSuchThing","ioe_filename":"R:\\Vids\\SystemDesign\\Wrong.txt","ioe_errno":"Just 2"}}}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough2",120]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough2",130],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"Message","contents":"Hi"}
  {"tag":"Warning","contents":"a warning"}
  {"tag":"IOAction","contents":"This is an arbitrary Put Line"}
  {"tag":"Message'","contents":{"message":"Hi there","info":"a verry long message dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}
  {"tag":"Warning'","contents":{"message":"Hi there warning","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}
  {"tag":"Warning'","contents":{"message":"Hi there warning 2","info":"a verry long warning dfsdfdsfdsf dfdsf sdfdsf sdfds dsfsdf bsfdfsdvf"}}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Rough2",130],{"unApStateDisplay":"ApState\n  { itemId = 130\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = Right \"Pre ~ Post !!\"\n  }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Rough2",130],{"unDStateDisplay":"V { iidx10 = 1300 }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough2",130]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough2",140],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"Message","contents":"Hi"}
  {"tag":"Warning","contents":"a warning"}
  {"tag":"IOAction","contents":"This is an arbitrary Put Line"}
  {"tag":"IOAction","contents":"This is an arbitrary THING THAT WILL BLOW UP"}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Rough2",140],{"unApStateDisplay":"ApState\n  { itemId = 140\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\VidList.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = Right \"Pre ~ Post !!\"\n  }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Rough2",140],{"unDStateDisplay":"V { iidx10 = 1400 }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough2",140]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Rough2",150],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\Vid List.txt","checks":[],"post":"Post"}]}
  {"tag":"Message","contents":"Hi"}
  {"tag":"Warning","contents":"a warning"}
  {"tag":"IOAction","contents":"This is an arbitrary Put Line"}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Rough2",150],{"unApStateDisplay":"ApState\n  { itemId = 150\n  , filePath = \"C:\\\\Vids\\\\SystemDesign\\\\Vid List.txt\"\n  , exePath = \"NOT IMPLEMENTED\"\n  , fileText = Right \"Pre ~ Post !!\"\n  }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Rough2",150],{"unDStateDisplay":"V { iidx10 = 1500 }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Rough2",150]}
  {"tag":"EndTest","contents":"DemoProject.Test.Rough2"}
  {"tag":"StartTest","contents":{"testModAddress":"DemoProject.Test.Simple2","testTitle":"This Simple Test Only Uses Ensure Effects","testConfig":{"minDepth":"DeepRegression","address":"DemoProject.Test.Simple2","countries":["AU"],"active":true,"header":"This Simple Test Only Uses Ensure Effects","environments":["TST","UAT","PreProd"]}}}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple2",100],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":100,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":["iid is small","iid is big"],"post":"Post"}]}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Simple2",100],{"unApStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Simple2",100],{"unDStateDisplay":"ApState { itemId = 100 , simpleMessage = \"Success\" }"}]}
  {"tag":"CheckOutcome","contents":[["DemoProject.Test.Simple2",100],{"result":{"tag":"Pass"},"info":{"header":"iid is small","messageInfo":null}}]}
  {"tag":"CheckOutcome","contents":[["DemoProject.Test.Simple2",100],{"result":{"tag":"Fail"},"info":{"header":"iid is big","messageInfo":null}}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple2",100]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple2",110],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":110,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Simple2",110],{"unApStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Simple2",110],{"unDStateDisplay":"ApState { itemId = 110 , simpleMessage = \"Success\" }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple2",110]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple2",123],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":123,"pre":"Pre","path":"R:\\Vids\\SystemDesign\\Wrong.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorFailure","contents":[["DemoProject.Test.Simple2",123],{"tag":"AppEnsureError","contents":"Only even iids expected"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple2",123]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple2",130],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":130,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Simple2",130],{"unApStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Simple2",130],{"unDStateDisplay":"ApState { itemId = 130 , simpleMessage = \"Success\" }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple2",130]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple2",140],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":140,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Simple2",140],{"unApStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Simple2",140],{"unDStateDisplay":"ApState { itemId = 140 , simpleMessage = \"Success\" }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple2",140]}
  {"tag":"StartIteration","contents":[["DemoProject.Test.Simple2",150],{"unWhenClause":"Pre"},{"unThenClause":"Post"},{"iid":150,"pre":"Pre","path":"C:\\Vids\\SystemDesign\\VidList.txt","checks":[],"post":"Post"}]}
  {"tag":"InteractorSuccess","contents":[["DemoProject.Test.Simple2",150],{"unApStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}
  {"tag":"PrepStateSuccess","contents":[["DemoProject.Test.Simple2",150],{"unDStateDisplay":"ApState { itemId = 150 , simpleMessage = \"Success\" }"}]}
  {"tag":"EndIteration","contents":["DemoProject.Test.Simple2",150]}
  {"tag":"EndTest","contents":"DemoProject.Test.Simple2"}
  {"tag":"EndGroup","contents":{"unGroupTitle":"Group 2"}}
  {"tag":"EndRun"}|]