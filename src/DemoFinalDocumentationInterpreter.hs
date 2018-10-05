

module DemoFinalDocumentationInterpreter where

import           DemoConfig
import           DemoData
import           DemoRoughTest
import           DSL.Interpreter
import           Foundation.Extended
import           Runner

dummyPrepState r a = a
returnApState item apState valState = apState
returnValState item apState valState = valState
testSteps = steps test

-- -- Demos
demoDocument :: IO (Either AppError ValState, [String])
demoDocument = executeFileSystemDocument prepState $ interactor runConfig sampleItem
--
demoDocumentedAll :: Either FilterError [IO (Either AppError ValState, [String])]
demoDocumentedAll = runSteps returnValState runConfig testSteps executeFileSystemDocument  All
--
demoDocumentNoVal :: IO (Either AppError ApState, [String])
demoDocumentNoVal = executeFileSystemDocument (dummyPrepState runConfig) (interactor runConfig sampleItem)
--
demoDocumentedAllNoVal :: Either FilterError [IO (Either AppError ApState, [String])]
demoDocumentedAllNoVal = runSteps returnApState runConfig testSteps executeFileSystemDocument All
