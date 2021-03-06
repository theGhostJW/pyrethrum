


module FileLogging where 

import Common as C
import DSL.Logger
import Pyrelude as P
import Pyrelude.IO
import Text.Show.Pretty
import AuxFiles
import OrphanedInstances()
import qualified System.IO as SIO
import qualified Data.Map.Strict as M

showAndLogItems :: Show a => IO AbsDir -> [a] -> IO ()
showAndLogItems projRoot = showAndLogList projRoot "items"

showAndLogList :: Show a => IO AbsDir -> Text -> [a] -> IO ()
showAndLogList projRoot logSuffix items = 
      let 
        logSpec :: M.Map (Text, FileExt) ()
        logSpec = M.singleton (logSuffix, FileExt ".log") ()

        hndle :: IO (Either (FrameworkError e) HandleInfo)
        hndle = either
                  Left
                  (
                    maybe
                      (Left $ C.Error "showAndLogList - no Handle returned")
                      (Right . snd)
                    . head
                  ) 
                <$> logFileHandles projRoot logSpec

        log2Both :: SIO.Handle -> Text -> IO ()
        log2Both fileHndl lgStr = putLines SIO.stdout lgStr *> putLines fileHndl lgStr

        listItems :: SIO.Handle -> IO ()
        listItems h = sequence_ $ log2Both h . txtPretty <$> items
      in
        hndle >>=
                either pPrint (\HandleInfo{path, fileHandle} -> 
                                  listItems fileHandle `finally` SIO.hClose fileHandle
                                  *> putStrLn ""
                                  *> putStrLn "--- Log Files ---"
                                  *> putStrLn (toS . toFilePath $ path)
                                  *> putStrLn ""
                              )


logFileHandles :: forall a e. IO AbsDir -> M.Map (Text, FileExt) a -> IO (Either (FrameworkError e) [(a, HandleInfo)])
logFileHandles projRoot suffixExtensionMap = 
  let
    openHandle :: (Text, FileExt) -> a -> IO (Either (FrameworkError e) (a, HandleInfo))
    openHandle (suff, ext) a = 
      do 
        eHandInfo <- logFileHandle projRoot suff ext
        pure $ eitherf eHandInfo
                (Left . IOError' "Error creating log file" )
                (\hInfo -> Right (a, hInfo))

    openHandles :: IO [((Text, FileExt), Either (FrameworkError e) (a, HandleInfo))]
    openHandles = M.toList <$> M.traverseWithKey openHandle suffixExtensionMap
  in 
    do 
      hlst <- openHandles
      let 
        fstErr = find (isLeft . snd) hlst
        openHndls = rights (snd <$> hlst)
      maybef fstErr
        (pure $ Right openHndls)
        (
          \((sfx, _ext), fstErr') -> 
            do 
              traverse_ (hClose . fileHandle . snd) openHndls
              pure . Left $ AnnotatedError 
                              ("Failed to create log file with suffix: " <> sfx) 
                              $ fromLeft (C.Error "won't happen") fstErr'
        )