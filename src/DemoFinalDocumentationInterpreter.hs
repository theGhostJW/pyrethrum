

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
demoDocument :: (Either AppError ValState, [String])
demoDocument = executeFileSystemDocument prepState $ interactor runConfig sampleItem
--
demoDocumentedAll :: Either FilterError [(Either AppError ValState, [String])]
demoDocumentedAll = runTest returnValState runConfig testSteps executeFileSystemDocument  All
--
-- --
demoDocumentNoVal :: (Either AppError ApState, [String])
demoDocumentNoVal = executeFileSystemDocument (dummyPrepState runConfig) (interactor runConfig sampleItem)
-- --
demoDocumentedAllNoVal :: Either FilterError [(Either AppError ApState, [String])]
demoDocumentedAllNoVal = runTest returnApState runConfig testSteps executeFileSystemDocument All
