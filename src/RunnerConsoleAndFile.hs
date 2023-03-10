module RunnerConsoleAndFile where

import           Polysemy
import           Polysemy.Reader
import           Polysemy.State
import           Common
import           DSL.Logger
import           DSL.CurrentTime
import DSL.LogProtocol
import DSL.LogProtocol.PrettyPrint
import           Pyrelude as P hiding (app)
import           Pyrelude.IO as PIO
import           FileLogging
import qualified System.IO as S
import AuxFiles as A
import Data.Aeson (ToJSON(..))
import Data.Map as M hiding (mapMaybe)

jsonItemLogExt = ".jsoni" :: Text

ioRunToFile :: forall b e appEffs. (Show e, ToJSON e, Members '[CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] appEffs) =>
    IO AbsDir
    -> WantConsole
    -> LogStyle
    -> (forall a. (forall effs. Members [CurrentTime, Reader ThreadInfo, State LogIndex, Embed IO] effs => Sem (Logger e ': effs) a -> Sem effs a) -> Sem appEffs a -> IO (Either (FrameworkError e) a))
    -> Sem appEffs b
    -> IO (Either (FrameworkError e) [AbsFile])
ioRunToFile projRoot wantConsole docMode interpreter app =
  let
    handleSpec :: M.Map (Text, FileExt) (ThreadInfo -> LogIndex -> Time -> LogProtocolBase e -> Text)
    handleSpec = M.fromList [
                                (("raw", FileExt ".log"), prettyPrintLogProtocolWith docMode)
                              , (("raw", FileExt jsonItemLogExt), logStrJSONWith)
                            ]

    fileHandleInfo ::  IO (Either (FrameworkError e) [(ThreadInfo -> LogIndex -> Time -> LogProtocolBase e -> Text, HandleInfo)])
    fileHandleInfo = logFileHandles projRoot handleSpec

    printFilePaths :: [AbsFile] -> IO ()
    printFilePaths lsFiles = do
                                putStrLn ""
                                putStrLn "--- Log Files ---"
                                mapM_ (putStrLn . toS . toFilePath) lsFiles
                                putStrLn ""

    fileHandles :: IO (Either (FrameworkError e) [(Maybe AbsFile, ThreadInfo -> LogIndex -> Time -> LogProtocolBase e -> Text, S.Handle)])
    fileHandles = (((\(fn, HandleInfo {path, fileHandle}) -> (Just path, fn, fileHandle)) <$>) <$>) <$> fileHandleInfo

    closeFileHandles :: [S.Handle] -> IO ()
    closeFileHandles  = traverse_ S.hClose

    allHandles :: IO (Either (FrameworkError e) [(Maybe AbsFile, ThreadInfo -> LogIndex -> Time -> LogProtocolBase e -> Text, S.Handle)])
    allHandles = wantConsole == Console
                      ? (((Nothing, prettyPrintLogProtocolWith docMode, S.stdout) :) <$>) <$> fileHandles
                      $ fileHandles
  in
    do
      hndls <- allHandles
      hndls & either
        (pure . Left)
        (\fileLoggerHandles ->
          do
            let
              getFile (mfile, _, _) = mfile
              getHandle (_, _, h) = h
              fileRecs = P.filter (isJust . getFile) fileLoggerHandles
              logPths = mapMaybe getFile fileRecs
              fileHndles = getHandle <$> fileRecs
              closeHandles = closeFileHandles fileHndles
              loggerHandles = (\(_f, l, h) -> (l, h)) <$> fileLoggerHandles

              logger :: Members '[Embed IO, Reader ThreadInfo, State LogIndex, CurrentTime] effs => Sem (Logger e ': effs) a -> Sem effs a
              logger = logToHandles loggerHandles

            runResult <- interpreter logger app `P.finally` closeHandles

            either
              (\e -> print ("Error Encountered \n" <> show e) $> Left e)
              (const $ printFilePaths logPths $> Right logPths)
              runResult
        )