
module DemoFinalIOInterpreter where

import           DemoData
import           DemoRoughTest
import           DSL.Interpreter
import           Foundation.Extended
import qualified Prelude             as P
import           Runner


dummyPrepState r a = a
returnApState item apState valState = apState
returnValState item apState valState = valState

demoExecuteFileSystemInIONoVal :: IO (Either AppError ApState)
demoExecuteFileSystemInIONoVal = executeFileSystemInIO (dummyPrepState sampleRunConfig) (interactor sampleRunConfig sampleItem)

demoIOAllNoVal:: Either FilterError [IO (Either AppError ApState)]
demoIOAllNoVal = runTest returnApState sampleRunConfig runElements executeFileSystemInIO All

demoIOAllNoValRepl :: IO (Either FilterError [Either AppError ApState])
demoIOAllNoValRepl = replShow demoIOAllNoVal

-- Run in IO
replShow d = P.sequenceA $ P.sequenceA <$> d

demoExecuteFileSystemInIO :: IO (Either AppError ValState)
demoExecuteFileSystemInIO = undefined -- executeFileSystemInIO (prepState sampleRunConfig) (interactor sampleRunConfig sampleItem)

demoIOToValState :: Either FilterError [IO (Either AppError ValState)]
demoIOToValState = runTest returnValState sampleRunConfig runElements executeFileSystemInIO All

demoIOAllToValStateRepl :: IO (Either FilterError [Either AppError ValState])
demoIOAllToValStateRepl = replShow demoIOToValState

demoIOFull :: Either FilterError [IO (Either AppError (TestInfo TestItem ApState ValState))]
demoIOFull = runFullTest sampleRunConfig runElements executeFileSystemInIO All

demoIOFullRepl :: IO (Either FilterError [Either AppError (TestInfo TestItem ApState ValState)])
demoIOFullRepl = replShow demoIOFull
