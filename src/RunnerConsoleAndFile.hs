module RunnerConsoleAndFile where

import           Polysemy
import           Polysemy.Reader
import           Polysemy.State
import           Polysemy.Resource
import Polysemy.Error as PE
import           Common
import           DSL.Interpreter
import           DSL.Logger
import           DSL.Ensure
import           DSL.CurrentTime
import DSL.LogProtocol
import DSL.LogProtocol.PrettyPrint
import           Pyrelude as P hiding (finally)
import           Pyrelude.IO as PIO 
import           Runner as R
import qualified System.IO as S
import qualified Control.Exception as E
import AuxFiles as A
import LogTransformation (prepareFinalLogs)
import Data.Aeson (ToJSON(..))
import Data.Map as M
import TestFilter


data WantConsole = Console | NoConsole deriving Eq
jsonItemLogExt = ".jsoni" :: Text

ioRunToFile :: forall tc rc effs. (
    RunConfigClass rc, 
    TestConfigClass tc, 
    Members '[Embed IO, Logger, Reader ThreadInfo, State LogIndex, Resource, Ensure, Error EnsureError, Error AppError, CurrentTime] effs
    ) =>
    WantConsole
    -> Bool 
    -> rc
    -> [TestFilter rc tc]
    -> (forall m1 m a. TestPlanBase tc rc m1 m a effs) 
    -> (forall as ds i. (ItemClass i ds, Show as, Show ds, ToJSON as, ToJSON ds) => (ItemRunParams as ds i tc rc effs -> Sem effs ()))
    -> Sem effs (Either AppError [AbsFile])
ioRunToFile wantConsole docMode rc testFilters pln itemRunner = 
  let 
    handleSpec :: M.Map (Text, FileExt) (ThreadInfo -> LogIdxTime -> LogProtocol -> Text) 
    handleSpec = M.fromList [
                                (("raw", FileExt ".log"), prettyPrintLogProtocolWith docMode)
                              , (("raw", FileExt jsonItemLogExt), logStrJSONWith)
                            ]

    fileHandleInfo :: IO (Either AppError [(ThreadInfo -> LogIdxTime -> LogProtocol -> Text, HandleInfo)])
    fileHandleInfo = logFileHandles handleSpec

    printFilePaths :: [AbsFile] -> IO ()
    printFilePaths lsFiles = do 
                                putStrLn ""
                                putStrLn "--- Log Files ---"
                                sequence_ $ putStrLn . toS . toFilePath <$> lsFiles
                                putStrLn ""
                          
    fileHandles :: IO (Either AppError [(Maybe AbsFile, ThreadInfo -> LogIdxTime -> LogProtocol -> Text, S.Handle)])
    fileHandles = (((\(fn, fh) -> (Just $ A.path fh, fn, fileHandle fh)) <$>) <$>) <$> fileHandleInfo

    closeFileHandles :: [S.Handle] -> IO ()
    closeFileHandles hdls = sequence_ $ S.hClose <$> hdls

    allHandles :: IO (Either AppError [(Maybe AbsFile, ThreadInfo -> LogIdxTime -> LogProtocol -> Text, S.Handle)])
    allHandles = wantConsole == Console
                      ? (((Nothing, prettyPrintLogProtocolWith docMode, S.stdout) :) <$>) <$> fileHandles
                      $ fileHandles
    
    executeRun :: [(ThreadInfo -> LogIdxTime -> LogProtocol -> Text, S.Handle)] -> Sem effs ()
    executeRun targHndls = testRun pln testFilters rc itemRunner

  in 
    do 
      hndls <- embed allHandles
      eitherf hndls
        (pure . Left)
        (\fileLoggerHandles -> 
          do 
            let 
              getFile (mfile, _, _) = mfile
              getHandle (_, _, h) = h
              fileRecs = P.filter (isJust . getFile) fileLoggerHandles
              logPths = catMaybes $ getFile <$> fileRecs
              fileHndles = getHandle <$> fileRecs
              closeHandles = embed $ closeFileHandles fileHndles
            
            executeRun ((\(_file, loggerFunc, handl) -> (loggerFunc, handl)) <$> fileLoggerHandles) `finally` closeHandles

            embed $ printFilePaths logPths 
            pure $ Right logPths
        )
