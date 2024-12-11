module Discover where

import WeederLibCopy.Weeder.Main qualified as W
import WeederLibCopy.Weeder qualified as W

-- glob
import qualified System.FilePath.Glob as Glob
import Data.Text as TXT
import GHC.Iface.Ext.Types (HieFile (hie_hs_file), hieVersion)
import GHC.Types.Name.Cache
import GHC.Iface.Ext.Binary (HieFileResult(..), readHieFileWithVersion)
import BasePrelude as BP hiding (show) 
import Prelude as P
import qualified Data.List
import System.FilePath (isExtSeparator)
import Control.Concurrent.Async (async, ExceptionInLinkedThread (..), link)


{-
- list modules ending in Test
- or Hook
- extract Fixtures and depended on hooks
- extract hook dependencies 
- validate??
- generate main
-}

-- $> discover
discover :: IO ()
discover = do
  P.putStrLn "Discovering..."
  -- P.withArgs [] W.main

moduleTarget :: Text
moduleTarget = "SuiteRuntimeTest"



-- | Find and read all .hie files in the given directories according to the given parameters,
-- exiting if any are incompatible with the current version of GHC.
-- The .hie files are returned as a lazy stream in the form of a list.
--
-- Will rethrow exceptions as 'ExceptionInLinkedThread' to the calling thread.
getHieFiles :: String -> [FilePath] -> Bool -> IO [HieFile]
getHieFiles hieExt hieDirectories requireHsFiles = do
  let hiePat = "**/*." <> hieExtNoSep
      hieExtNoSep = if isExtSeparator (Data.List.head hieExt) then Data.List.tail hieExt else hieExt

  hieFilePaths :: [FilePath] <-
    P.concat <$>
      traverse ( getFilesIn hiePat )
        ( if BP.null hieDirectories
          then ["./."]
          else hieDirectories
        )

  hsFilePaths :: [FilePath] <-
    if requireHsFiles
      then getFilesIn "**/*.hs" "./."
      else pure []

  hieFileResultsChan <- newChan

  nameCache <-
    initNameCache 'z' []

  a <- async $ handleWeederException do
    readHieFiles nameCache hieFilePaths hieFileResultsChan hsFilePaths
    writeChan hieFileResultsChan Nothing
 
  link a

  catMaybes P.. P.takeWhile isJust <$> getChanContents hieFileResultsChan

  where

    readHieFiles nameCache hieFilePaths hieFileResultsChan hsFilePaths =
      BP.for_ hieFilePaths \hieFilePath -> do
        hieFileResult <-
          readCompatibleHieFileOrExit nameCache hieFilePath
        let hsFileExists = P.any ( hie_hs_file hieFileResult `BP.isSuffixOf` ) hsFilePaths
        when (requireHsFiles ==> hsFileExists) $
          writeChan hieFileResultsChan (Just hieFileResult)


-- | Recursively search for files with the given extension in given directory
getFilesIn
  :: String
  -- ^ Only files matching this pattern are considered.
  -> FilePath
  -- ^ Directory to look in
  -> IO [FilePath]
getFilesIn pat root = do
  [result] <- Glob.globDir [Glob.compile pat] root
  pure result
  

-- | Read a .hie file, exiting if it's an incompatible version.
readCompatibleHieFileOrExit :: NameCache -> FilePath -> IO HieFile
readCompatibleHieFileOrExit nameCache path = do
  res <- readHieFileWithVersion (\(v, _) -> v == hieVersion) nameCache path
  case res of
    Right HieFileResult{ hie_file_result } ->
      return hie_file_result
    Left ( v, _ghcVersion ) ->
      throwIO $ ExitHieVersionFailure path v


infixr 5 ==>


-- | An infix operator for logical implication
(==>) :: Bool -> Bool -> Bool
True  ==> x = x
False ==> _ = True


-- | Convert 'WeederException' to the corresponding 'ExitCode' and emit an error 
-- message to stderr.
--
-- Additionally, unwrap 'ExceptionInLinkedThread' exceptions: this is for
-- 'getHieFiles'.
handleWeederException :: IO a -> IO a
handleWeederException a = catches a handlers 
  where
    handlers = [ Handler rethrowExits
               , Handler unwrapLinks
               ]
    rethrowExits w = do
      hPutStrLn stderr (displayException w)
      BP.exitWith (weederExitCode w)
    unwrapLinks (ExceptionInLinkedThread _ (SomeException w)) =
      throwIO w


weederExitCode :: WeederException -> ExitCode
weederExitCode = \case
  ExitWeedsFound -> ExitFailure 228
  ExitHieVersionFailure _ _ -> ExitFailure 2
  ExitConfigFailure _ -> ExitFailure 3
  ExitNoHieFilesFailure -> ExitFailure 4

-- | Each exception corresponds to an exit code.
data WeederException 
  = ExitNoHieFilesFailure
  | ExitHieVersionFailure 
      FilePath -- ^ Path to HIE file
      Integer -- ^ HIE file's header version
  | ExitConfigFailure
      String -- ^ Error message
  | ExitWeedsFound
  deriving Show

instance Exception WeederException where
  displayException = \case
    ExitNoHieFilesFailure -> noHieFilesFoundMessage
    ExitHieVersionFailure path v -> hieVersionMismatchMessage path v
    ExitConfigFailure s -> s
    ExitWeedsFound -> mempty
    where

      noHieFilesFoundMessage =  
        "No HIE files found: check that the directory is correct "
        <> "and that the -fwrite-ide-info compilation flag is set."

      hieVersionMismatchMessage path v = Data.List.unlines
        [ "incompatible hie file: " <> path
        , "    this version of weeder was compiled with GHC version "
          <> show hieVersion
        , "    the hie files in this project were generated with GHC version "
          <> show v
        , "    weeder must be built with the same GHC version"
          <> " as the project it is used on"
        ]


