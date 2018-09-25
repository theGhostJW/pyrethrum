
module DemoFinalIOInterpreter where

import           DemoConfig
import           DemoData
import           DemoRoughTest
import           DSL.Interpreter
import           Foundation.Extended hiding (Item)
import qualified Prelude             as P
import           Runner


dummyPrepState r a = a
returnApState item apState valState = apState
returnValState item apState valState = valState
testSteps = steps test

demoExecuteFileSystemInIONoVal :: IO (Either AppError ApState)
demoExecuteFileSystemInIONoVal = executeFileSystemInIO (dummyPrepState runConfig) (interactor runConfig sampleItem)

demoIOAllNoVal:: Either FilterError [IO (Either AppError ApState)]
demoIOAllNoVal = runTest returnApState runConfig testSteps executeFileSystemInIO All

demoIOAllNoValRepl :: IO (Either FilterError [Either AppError ApState])
demoIOAllNoValRepl = replShow demoIOAllNoVal

-- Run in IO
replShow d = P.sequenceA $ P.sequenceA <$> d

demoExecuteFileSystemInIO :: IO (Either AppError ValState)
demoExecuteFileSystemInIO = undefined -- executeFileSystemInIO (prepState sampleRunConfig) (interactor sampleRunConfig sampleItem)

demoIOToValState :: Either FilterError [IO (Either AppError ValState)]
demoIOToValState = runTest returnValState runConfig testSteps executeFileSystemInIO All

demoIOAllToValStateRepl :: IO (Either FilterError [Either AppError ValState])
demoIOAllToValStateRepl = replShow demoIOToValState

demoIOFull :: Either FilterError [IO (Either AppError (TestInfo Item ApState ValState))]
demoIOFull = runFullTest runConfig testSteps executeFileSystemInIO All

demoIOFullRepl :: IO (Either FilterError [Either AppError (TestInfo Item ApState ValState)])
demoIOFullRepl = replShow demoIOFull