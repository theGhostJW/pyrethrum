

module DemoFinalDocumentationInterpreter where

import           DemoRoughTest
import           DSL.Interpreter
import           Foundation.Extended
import           Runner

dummyPrepState r a = a
returnApState item apState valState = apState
returnValState item apState valState = valState

-- -- Demos
demoDocument :: (Either AppError ValState, [String])
demoDocument = executeFileSystemDocument prepState $ interactor sampleRunConfig sampleItem
--
demoDocumentedAll :: Either FilterError [(Either AppError ValState, [String])]
demoDocumentedAll = runTest returnValState sampleRunConfig runElements executeFileSystemDocument  All
--
-- --
demoDocumentNoVal :: (Either AppError ApState, [String])
demoDocumentNoVal = executeFileSystemDocument (dummyPrepState sampleRunConfig) (interactor sampleRunConfig sampleItem)
-- --
demoDocumentedAllNoVal :: Either FilterError [(Either AppError ApState, [String])]
demoDocumentedAllNoVal = runTest returnApState sampleRunConfig runElements executeFileSystemDocument All

demoExecuteFileSystemInIONoVal :: IO (Either AppError ApState)
demoExecuteFileSystemInIONoVal = executeFileSystemInIO (dummyPrepState sampleRunConfig) (interactor sampleRunConfig sampleItem)
