{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}

-- | This module provides an entry point to the Weeder executable.

module WeederLibCopy.Weeder.Main ( main, mainWithConfig, discover ) where

-- base
import Control.Exception ( throwIO )
import Control.Monad ( guard, unless )
import Data.Foldable
import Data.List ( isSuffixOf, sortOn )
import Data.Version ( showVersion )
import System.Exit ( exitFailure, ExitCode(..), exitWith )
import System.IO ( stderr, hPutStrLn )

-- containers
import Data.Map.Strict qualified as Map
import Data.Set ( Set )
import Data.Set qualified as Set

-- toml-reader
import TOML qualified

-- directory
import System.Directory ( canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory )

-- filepath
import System.FilePath ( isExtensionOf )

-- ghc
import GHC.Iface.Ext.Binary ( HieFileResult( HieFileResult, hie_file_result ), readHieFileWithVersion )
import GHC.Iface.Ext.Types ( HieFile(.. ), hieVersion, HieASTs (..), HieAST, TypeIndex )
import GHC.Unit.Module ( moduleName, moduleNameString, GenModule (..) )
import GHC.Types.Name.Cache ( initNameCache, NameCache )
import GHC.Types.Name ( occNameString )
import GHC.Types.SrcLoc ( RealSrcLoc, realSrcSpanStart, srcLocLine )
import Text.Show.Pretty (PrettyVal (prettyVal), pPrint, pPrintList, ppDocList, ppShow, ppShowList)

-- regex-tdfa
import Text.Regex.TDFA ( (=~) )

-- optparse-applicative
import Options.Applicative

-- text
import Data.Text.IO qualified as T

-- transformers
import Control.Monad.Trans.State.Strict ( execStateT )

-- weeder
import WeederLibCopy.Weeder
import WeederLibCopy.Weeder.Config
import Paths_pyrethrum (version)
import BasePrelude ((&))
import Debug.Trace
import Data.Text (Text, intercalate, isInfixOf)
import PyrethrumExtras (uu, txt, toS)
import GHC.Plugins (Outputable(..), renderWithContext, defaultSDocContext)


data CLIArguments = CLIArguments
  { configPath :: FilePath
  , hieExt :: String
  , hieDirectories :: [FilePath]
  , requireHsFiles :: Bool
  , writeDefaultConfig :: Bool
  , noDefaultFields :: Bool
  }


parseCLIArguments :: Parser CLIArguments
parseCLIArguments = do
    configPath <- strOption
        ( long "config"
            <> help "A file path for Weeder's configuration."
            <> value "./weeder.toml"
            <> metavar "<weeder.toml>"
        )
    hieExt <- strOption
        ( long "hie-extension"
            <> value ".hie"
            <> help "Extension of HIE files"
            <> showDefault
        )
    hieDirectories <- many (
        strOption
            ( long "hie-directory"
                <> help "A directory to look for .hie files in. Maybe specified multiple times. Default ./."
            )
        )
    requireHsFiles <- switch
          ( long "require-hs-files"
              <> help "Skip stale .hie files with no matching .hs modules"
          )
    writeDefaultConfig <- switch
          ( long "write-default-config"
              <> help "Write a default configuration file if the one specified by --config does not exist"
          )
    noDefaultFields <- switch
          ( long "no-default-fields"
              <> help "Do not use default field values for missing fields in the configuration."
          )
    pure CLIArguments{..}


-- | Parse command line arguments and into a 'Config' and run 'mainWithConfig'.
main :: IO ()
main = do
  CLIArguments{..} <-
    execParser $
      info (parseCLIArguments <**> helper <**> versionP) mempty

  configExists <-
    doesFileExist configPath

  unless (writeDefaultConfig ==> configExists) do
    hPutStrLn stderr $ "Did not find config: wrote default config to " ++ configPath
    writeFile configPath (configToToml defaultConfig)

  (exitCode, _) <-
    decodeConfig noDefaultFields configPath
      >>= either throwIO pure
      >>= mainWithConfig hieExt hieDirectories requireHsFiles

  exitWith exitCode
  where
    decodeConfig noDefaultFields =
      if noDefaultFields
        then fmap (TOML.decodeWith decodeNoDefaults) . T.readFile
        else TOML.decodeFile
    versionP = infoOption ( "weeder version "
                            <> showVersion version
                            <> "\nhie version "
                            <> show hieVersion )
        ( long "version" <> help "Show version" )


-- | Run Weeder in the current working directory with a given 'Config'.
--
-- This will recursively find all files with the given extension in the given directories, perform
-- analysis, and report all unused definitions according to the 'Config'.
mainWithConfig :: String -> [FilePath] -> Bool -> Config -> IO (ExitCode, Analysis)
mainWithConfig hieExt hieDirectories requireHsFiles weederConfig@Config{ rootPatterns, typeClassRoots, rootInstances } = do
  hieFilePaths <-
    concat <$>
      traverse ( getFilesIn hieExt )
        ( if null hieDirectories
          then ["./."]
          else hieDirectories
        )

  hsFilePaths <-
    if requireHsFiles
      then getFilesIn ".hs" "./."
      else pure []

  nameCache <-
    initNameCache 'z' []

  hieFileResults <-
    mapM ( readCompatibleHieFileOrExit nameCache ) hieFilePaths

  let
    hieFileResults' = flip filter hieFileResults \hieFileResult ->
      let hsFileExists = any ( hie_hs_file hieFileResult `isSuffixOf` ) hsFilePaths
       in requireHsFiles ==> hsFileExists

  analysis <-
    execStateT ( analyseHieFiles weederConfig hieFileResults' ) emptyAnalysis

  let
    roots =
      Set.filter
        ( \d ->
            any
              ( displayDeclaration d =~ )
              rootPatterns
        )
        ( allDeclarations analysis )

    reachableSet =
      reachable
        analysis
        ( Set.map DeclarationRoot roots <> filterImplicitRoots analysis ( implicitRoots analysis ) )

    dead =
      allDeclarations analysis Set.\\ reachableSet

    warnings =
      Map.unionsWith (++) $
      foldMap
        ( \d ->
            fold $ do
              moduleFilePath <- Map.lookup ( declModule d ) ( modulePaths analysis )
              spans <- Map.lookup d ( declarationSites analysis )
              guard $ not $ null spans
              let starts = map realSrcSpanStart $ Set.toList spans
              return [ Map.singleton moduleFilePath ( liftA2 (,) starts (pure d) ) ]
        )
        dead

  for_ ( Map.toList warnings ) \( path, declarations ) ->
    for_ (sortOn (srcLocLine . fst) declarations) \( start, d ) ->
      case Map.lookup d (prettyPrintedType analysis) of
        Nothing -> putStrLn $ showWeed path start d
        Just t -> putStrLn $ showPath path start <> "(Instance) :: " <> t

  let exitCode = if null warnings then ExitSuccess else ExitFailure 1

  pure (exitCode, analysis)

  where

    filterImplicitRoots :: Analysis -> Set Root -> Set Root
    filterImplicitRoots Analysis{ prettyPrintedType, modulePaths } = Set.filter $ \case
      DeclarationRoot _ -> True -- keep implicit roots for rewrite rules etc

      ModuleRoot _ -> True

      InstanceRoot d c -> typeClassRoots || matchingType
        where
          matchingType =
            let mt = Map.lookup d prettyPrintedType
                matches = maybe (const False) (=~) mt
            in any (maybe True matches) filteredInstances

          filteredInstances :: Set (Maybe String)
          filteredInstances =
            Set.map instancePattern
            . Set.filter (maybe True (displayDeclaration c =~) . classPattern)
            . Set.filter (maybe True modulePathMatches . modulePattern)
            $ rootInstances

          modulePathMatches :: String -> Bool
          modulePathMatches p = maybe False (=~ p) (Map.lookup ( declModule d ) modulePaths)


displayDeclaration :: Declaration -> String
displayDeclaration d =
  moduleNameString ( moduleName ( declModule d ) ) <> "." <> occNameString ( declOccName d )


showWeed :: FilePath -> RealSrcLoc -> Declaration -> String
showWeed path start d =
  showPath path start
    <> occNameString ( declOccName d)


showPath :: FilePath -> RealSrcLoc -> String
showPath path start =
  path <> ":" <> show ( srcLocLine start ) <> ": "


-- | Recursively search for files with the given extension in given directory
getFilesIn
  :: String
  -- ^ Only files with this extension are considered
  -> FilePath
  -- ^ Directory to look in
  -> IO [FilePath]
getFilesIn ext path = do
  exists <-
    doesPathExist path

  if exists
    then do
      isFile <-
        doesFileExist path

      if isFile && ext `isExtensionOf` path
        then do
          path' <-
            canonicalizePath path

          return [ path' ]

        else do
          isDir <-
            doesDirectoryExist path

          if isDir
            then do
              cnts <-
                listDirectory path

              withCurrentDirectory path ( foldMap ( getFilesIn ext ) cnts )

            else
              return []

    else
      return []


-- | Read a .hie file, exiting if it's an incompatible version.
readCompatibleHieFileOrExit :: NameCache -> FilePath -> IO HieFile
readCompatibleHieFileOrExit nameCache path = do
  res <- readHieFileWithVersion (\(v, _) -> v == hieVersion) nameCache path
  case res of
    Right HieFileResult{ hie_file_result } ->
      return hie_file_result
    Left ( v, _ghcVersion ) -> do
      putStrLn $ "incompatible hie file: " <> path
      putStrLn $ "    this version of weeder was compiled with GHC version "
               <> show hieVersion
      putStrLn $ "    the hie files in this project were generated with GHC version "
               <> show v
      putStrLn $ "    weeder must be built with the same GHC version"
               <> " as the project it is used on"
      exitFailure


infixr 5 ==>


-- | An infix operator for logical implication
(==>) :: Bool -> Bool -> Bool
True  ==> x = x
False ==> _ = True



displayHieAst :: HieAST TypeIndex -> Text
displayHieAst ast = toS . renderWithContext defaultSDocContext $ ppr ast

data DecShow = DecShow {
  path :: Text,
  decs :: Text
} deriving Show

displayInfo :: HieFile -> Text
displayInfo HieFile {hie_hs_file, hie_module = hie_module@Module {
  moduleUnit, moduleName
}, hie_types, hie_asts, hie_hs_src } =
  -- intercalate ", " $ txt <$> paths
  -- txt . ppShow $ astDs -- hangs
  toS . ppShowList $ decs2 -- module path
  --  str <- lookupPprType t 
 where
  asts = getAsts hie_asts
  paths = Map.keys asts
  decs = findDeclarations <$> asts
  justEg =  Map.filterWithKey (\k _ -> isInfixOf "DemoTest" $ txt k) decs
  decs2 =  Map.mapWithKey (\k v -> DecShow (txt k) (toS $ ppShowList v)) decs
  astDs = Map.mapWithKey (\k v ->
     DecShow (txt k) (displayHieAst v)
    ) asts


-- My Non Weeder Code
-- discover :: IO (ExitCode, Analysis)
discover :: IO ()
discover = do
  hieFilePaths <- concat <$> traverse ( getFilesIn ".hie" ) ["./."]
  hsFilePaths <- getFilesIn ".hs" "./."
  nameCache <- initNameCache 'z' []

  hieFiles <-
    mapM ( readCompatibleHieFileOrExit nameCache ) hieFilePaths

  let
    filteredHieFiles = 
      flip filter hieFiles \hieFile -> (isInfixOf "DemoTest" . toS $ hie_hs_file hieFile) && any ( hie_hs_file hieFile `isSuffixOf`) hsFilePaths

  traverse_ (pPrint . displayInfo) filteredHieFiles
  -- analysis <-
  --   execStateT ( analyseHieFilesDiscover hieFileResults' ) emptyAnalysis

  -- let
  --   roots = allDeclarations analysis

  --   reachableSet =
  --     reachable
  --       analysis
  --       ( Set.map DeclarationRoot roots <> filterImplicitRoots analysis ( implicitRoots analysis ) )

  --   dead =
  --     allDeclarations analysis Set.\\ reachableSet

  --   warnings =
  --     Map.unionsWith (++) $
  --     foldMap
  --       ( \d ->
  --           fold $ do
  --             moduleFilePath <- Map.lookup ( declModule d ) ( modulePaths analysis )
  --             spans <- Map.lookup d ( declarationSites analysis )
  --             guard $ not $ null spans
  --             let starts = map realSrcSpanStart $ Set.toList spans
  --             return [ Map.singleton moduleFilePath ( liftA2 (,) starts (pure d) ) ]
  --       )
  --       dead

  -- for_ ( Map.toList warnings ) \( path, declarations ) ->
  --   for_ (sortOn (srcLocLine . fst) declarations) \( start, d ) ->
  --     case Map.lookup d (prettyPrintedType analysis) of
  --       Nothing -> putStrLn $ showWeed path start d
  --       Just t -> putStrLn $ showPath path start <> "(Instance) :: " <> t

  -- let exitCode = if null warnings then ExitSuccess else ExitFailure 1

  -- pure (exitCode, analysis)

  where

    filterImplicitRoots :: Analysis -> Set Root -> Set Root
    filterImplicitRoots Analysis{ prettyPrintedType, modulePaths } = Set.filter $ \case
      DeclarationRoot _ -> True -- keep implicit roots for rewrite rules etc

      ModuleRoot _ -> True

      InstanceRoot d c -> True