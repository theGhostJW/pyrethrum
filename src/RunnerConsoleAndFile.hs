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
import           Pyrelude as P hiding (app)
import           Pyrelude.IO as PIO 
import           Runner as R
import qualified System.IO as S
import qualified Control.Exception as E
import AuxFiles as A
import LogTransformation (prepareFinalLogs)
import Data.Aeson (ToJSON(..))
import Data.Map as M
import TestFilter

jsonItemLogExt = ".jsoni" :: Text

ioRunToFile :: forall b appEffs. Members '[CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] appEffs =>
    WantConsole
    -> Bool 
    -> (forall a. (forall effs. Members [CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs => Sem (Logger ': effs) a -> Sem effs a) -> Sem appEffs a -> IO (Either AppError a))
    -> Sem appEffs b
    -> IO (Either AppError [AbsFile])
ioRunToFile wantConsole docMode interpreter app = 
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
    closeFileHandles  = traverse_ S.hClose

    allHandles :: IO (Either AppError [(Maybe AbsFile, ThreadInfo -> LogIdxTime -> LogProtocol -> Text, S.Handle)])
    allHandles = wantConsole == Console
                      ? (((Nothing, prettyPrintLogProtocolWith docMode, S.stdout) :) <$>) <$> fileHandles
                      $ fileHandles
  in 
    do 
      hndls <- allHandles
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
              closeHandles = closeFileHandles fileHndles
              loggerHandles = (\(f, l, h) -> (l, h)) <$> fileLoggerHandles

              logger :: Members '[Embed IO, Reader ThreadInfo, State LogIndex, CurrentTime] effs => Sem (Logger ': effs) a -> Sem effs a
              logger = logToHandles loggerHandles
              
            runResult <- interpreter logger app `P.finally` closeHandles
            
            eitherf runResult 
              (\e -> print ("Error Encountered \n" <> show e) $> Left e) 
              (const $ printFilePaths logPths $> Right logPths)
        )