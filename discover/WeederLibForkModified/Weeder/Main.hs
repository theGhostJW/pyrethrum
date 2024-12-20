{-# language ApplicativeDo #-}
{-# language ScopedTypeVariables #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-x-orphans #-}
{-# OPTIONS_GHC -Wno-w-unused-top-binds #-}

-- | This module provides an entry point to the Weeder executable.

module WeederLibForkModified.Weeder.Main ( main, mainWithConfig, getHieFiles ) where

-- async
import Control.Concurrent.Async ( async, link, ExceptionInLinkedThread ( ExceptionInLinkedThread ) )

-- base
import Control.Exception ( throwIO, catches, Handler ( Handler ))
import Control.Concurrent ( getChanContents, newChan, writeChan, setNumCapabilities )
import Data.List
import Data.Map.Strict qualified as M
import Data.Version ( showVersion, Version (Version) )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( hPutStrLn )

-- jw
import Data.Text.IO qualified as TIO
import PyrethrumExtras qualified as PE

-- toml-reader
import qualified TOML

-- directory
import System.Directory ( doesFileExist )

-- filepath
import System.FilePath ( isExtSeparator )

-- glob
import qualified System.FilePath.Glob as Glob

-- ghc
import GHC.Iface.Ext.Binary ( HieFileResult( HieFileResult, hie_file_result ), readHieFileWithVersion )
import GHC.Iface.Ext.Types ( HieFile( hie_hs_file ), hieVersion )
import GHC.Types.Name.Cache ( initNameCache, NameCache )

-- optparse-applicative
import Options.Applicative

-- text
import qualified Data.Text.IO as T
import Data.Set qualified as S

-- weeder
import WeederLibForkModified.Weeder.Run
import WeederLibForkModified.Weeder.Config
import WeederLibForkModified.Weeder
import GHC.Types.Name (occNameString)
import GHC.Generics (Datatype(..))
import qualified GHC as Module
import GHC.Unit.Module as UM (moduleNameString, moduleName) 
import Data.Text qualified as T
import Internal.LogQueries (isTest, isHook)
import GHC.Unit.Types (Module)

-- replace weeder version to get compiling - was relying on paths
version :: Version
version = Version [0, 1, 0, 0] []

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


weederExitCode :: WeederException -> ExitCode
weederExitCode = \case
  ExitWeedsFound -> ExitFailure 228
  ExitHieVersionFailure _ _ -> ExitFailure 2
  ExitConfigFailure _ -> ExitFailure 3
  ExitNoHieFilesFailure -> ExitFailure 4

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
      System.Exit.exitWith (weederExitCode w)
    unwrapLinks (ExceptionInLinkedThread _ (SomeException w)) =
      throwIO w
data CLIArguments = CLIArguments
  { configPath :: FilePath
  , hieExt :: String
  , hieDirectories :: [FilePath]
  , requireHsFiles :: Bool
  , writeDefaultConfig :: Bool
  , noDefaultFields :: Bool
  , capabilities :: Maybe Int
  } deriving Show

-- $> main 
main :: IO ()
main = handleWeederException do
  let config =
       Config {
              rootPatterns = [ "Main.main", "^Paths_.*"]
            , typeClassRoots = False
            , rootInstances = [ ClassOnly "\\.IsString$", ClassOnly "\\.IsList$" ]
            , unusedTypes = False
            , rootModules = mempty
            } :: ConfigParsed
      hieDirectories = []
      hieExt = ".hie"
      requireHsFiles = True
      capabilities = 1

  setNumCapabilities capabilities
  compileConfig config &
      either (throwIO . ExitConfigFailure) (mainWithConfig hieExt hieDirectories requireHsFiles)

data Export = MkExport { moduleName :: Text, exportName :: [Text]}
  deriving (Show, Eq, Ord)

-- | Run Weeder in the current working directory with a given 'Config'.
--
-- This will recursively find all files with the given extension in the given directories, perform
-- analysis, and report all unused definitions according to the 'Config'.
--
-- Exits with one of the listed Weeder exit codes on failure.
mainWithConfig :: String -> [FilePath] -> Bool -> Config -> IO ()
mainWithConfig hieExt hieDirectories requireHsFiles weederConfig = handleWeederException do
  hieFiles <-
    getHieFiles hieExt hieDirectories requireHsFiles

  when (null hieFiles) $ throwIO ExitNoHieFilesFailure

  let
    (weeds, analysis :: Analysis) =
      runWeeder weederConfig hieFiles
    filterTransformExports expSet = S.filter filterRule $ S.map (\exs -> PE.toS $ occNameString exs.declOccName) expSet
    exportModuleMap = filterTransformExports <$> M.filterWithKey (\k _v -> filterRule k) (M.mapKeys showModuleName analysis.exports) 
    dependencyGraph = analysis.dependencyGraph


  let !filteredExports = PE.db "filtered exports"  exportModuleMap
  print $ length filteredExports

  let logFile = "weeder.log"
  TIO.writeFile (PE.toS logFile) (PE.txt analysis)
  TIO.putStrLn logFile

  -- mapM_ (putStrLn . formatWeed) weeds
  -- unless (null weeds) $ throwIO ExitWeedsFound

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
    concat <$>
      traverse ( getFilesIn hiePat )
        ( if null hieDirectories
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

  catMaybes . takeWhile isJust <$> getChanContents hieFileResultsChan

  where

    readHieFiles nameCache hieFilePaths hieFileResultsChan hsFilePaths =
      for_ hieFilePaths \hieFilePath -> do
        hieFileResult <-
          readCompatibleHieFileOrExit nameCache hieFilePath
        let hsFileExists = any ( hie_hs_file hieFileResult `isSuffixOf` ) hsFilePaths
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
