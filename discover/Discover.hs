-- Weeder uses head and tail
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE DeriveAnyClass #-}

module Discover where


-- glob
import qualified System.FilePath.Glob as Glob
import Data.Text as TXT
import GHC.Iface.Ext.Types (HieFile (..), hieVersion, TypeIndex, HieType (..), HieArgs (..), HiePath, HieAST (..), HieASTs (..), IdentifierDetails (..), ContextInfo, SourcedNodeInfo (getSourcedNodeInfo), nodeIdentifiers, HieTypeFix(..), NodeIdentifiers, NodeInfo)
import GHC.Types.Name.Cache
import GHC.Iface.Ext.Binary (HieFileResult(..), readHieFileWithVersion, readHieFile)
import BasePrelude (
 ExitCode (..),
 throwIO,
 hPutStrLn,
 Handler (..),
 catches,
 writeChan, newChan, getChanContents)
import BasePrelude qualified as BP
import Prelude as P
import qualified Data.List
import System.FilePath (isExtSeparator)
import Control.Concurrent.Async (async, ExceptionInLinkedThread (..), link)
-- import Text.Show.Pretty (pPrint)
import PyrethrumExtras qualified as PE
-- import PyrethrumExtras.Test qualified as PET
import GHC (Module, ModuleName, moduleNameString)
import GHC.Unit.Types (GenModule(..))
import GHC.Plugins (Outputable(..), SDoc, moduleStableString)
import GHC.Utils.Outputable (traceSDocContext, renderWithContext, showSDocOneLine, defaultSDocContext)
import GHC.Iface.Syntax (IfaceTyCon(..), pprIfaceSigmaType, ShowForAllFlag (..))
import Data.Map qualified as M
import GHC.Types.Avail (AvailInfo)
import Data.Text.IO qualified as TIO
import GHC.Types.Name
    ( OccName,
      Name,
      nameModule_maybe,
      isTvOcc,
      isVarOcc,
      isTcOcc,
      isDataOcc,
      isDataSymOcc,
      occNameString,
      nameOccName )
import GHC.Iface.Ext.Utils (recoverFullType, hieTypeToIface)
import qualified Data.Set as Set
import Debug.Trace.Extended (uu)
import qualified Data.IntMap.Merge.Lazy as M


{-
- list modules ending in Test
- or Hook
- extract Fixtures and depended on hooks
- extract hook dependencies 
- validate??
- generate main
-}

gptDiscover = do
  hieFiles <- getHieFiles "hie" ["./"] True
  P.putStrLn "Hie files found"
  let mainHiePath = (.hie_hs_file) $ BP.head $ P.filter (\f ->
                                   let
                                    moduleName = PE.txt f.hie_module.moduleName
                                   in
                                    "Minimal" `isInfixOf` moduleName
                                    -- ||
                                    -- "mBase" `isInfixOf` moduleName
                                     )  hieFiles
  let otherHiePaths = (.hie_hs_file) <$> P.filter (\f ->
                                   let
                                    moduleName = PE.txt f.hie_module.moduleName
                                   in
                                     "mBase" `isInfixOf` moduleName ||
                                     "DemoTest" `isInfixOf` moduleName
                                     )  hieFiles
  tree <- buildDependencyTree mainHiePath otherHiePaths
  P.print tree

-- >>> discover
-- "log file written: hieResultsMinimal.log"
discover :: IO Text
discover = do
  P.putStrLn "Discovering..."
  hieFiles <- getHieFiles "hie" ["./"] True
  P.putStrLn "Hie files found"
  let filesOfInterest = P.filter (\f ->
                                   let
                                    moduleName = PE.txt f.hie_module.moduleName
                                   in
                                    "Minimal" `isInfixOf` moduleName
                                    -- ||
                                    -- "mBase" `isInfixOf` moduleName
                                     )  hieFiles
      fileContent = P.concatMap txtHieFile2 filesOfInterest
      -- fileContent = P.concatMap txtHieFile filesOfInterest
      -- logFile = "hieResultsBase.log"
      logFile = "hieResultsMinimal.log"
      message = "log file written: " <> logFile
  TIO.writeFile (PE.toS logFile) (TXT.unlines fileContent)
  TIO.putStrLn message
  pure message

putLines :: Foldable t => t Text -> IO ()
putLines = traverse_ putTextLn

lookupType :: HieFile -> TypeIndex -> HieTypeFix
lookupType HieFile {hie_types}  t = recoverFullType t hie_types

renderType :: HieTypeFix -> String
renderType = showSDocOneLine defaultSDocContext . pprIfaceSigmaType ShowForAllWhen . hieTypeToIface

lookupRenderType :: HieFile -> TypeIndex -> Text
lookupRenderType hieFile t =
    PE.toS (renderType typ)
    <> "\n"
    <> "Names"
    <> "\n"
    <> renderNameSet (typeToNames typ)
   where typ = lookupType hieFile t


renderNameSet  :: Set Name -> Text
renderNameSet = P.unlines . fmap (renderUnlabled .  ppr) . Set.toList

-- | Names mentioned within the type.
typeToNames :: HieTypeFix -> Set Name
typeToNames (Roll t) = case t of
  HTyVarTy n -> Set.singleton n

  HAppTy a (HieArgs args) ->
    typeToNames a <> hieArgsTypes args

  HTyConApp (IfaceTyCon{ifaceTyConName}) (HieArgs args) ->
    Set.singleton ifaceTyConName <> hieArgsTypes args

  HForAllTy _ a -> typeToNames a

  HFunTy _mult b c ->
    typeToNames b <> typeToNames c

  HQualTy a b ->
    typeToNames a <> typeToNames b

  HLitTy _ -> mempty

  HCastTy a -> typeToNames a

  HCoercionTy -> mempty

  where

    hieArgsTypes :: [(Bool, HieTypeFix)] -> Set Name
    hieArgsTypes = foldMap (typeToNames . snd) . P.filter fst

txtHieFile :: HieFile -> [Text]
txtHieFile HieFile {
  hie_hs_file
  , hie_module
  , hie_types
  , hie_asts
  , hie_exports
  -- , hie_hs_src
}  =
  "---- HIE FILE ----" :
    PE.toS hie_hs_file :
     showModule hie_module
     <> (
      "---- TYPES ----" :
      (showHieType <$> toList hie_types)
     )
     <>
      ("---- HIE AST ----" :
       (showAst <$> M.toList hie_asts.getAsts)
      )
      <>
        ("---- HIE EXPORTS ----" :
        (showHIEExport <$> hie_exports)
        )
      -- <> ("---- HIE SOURCE ----"
      --   : TXT.lines (decodeUtf8 hie_hs_src)
      --   )
      <> ["---- END ----"]

txtHieFile2 :: HieFile -> [Text]
txtHieFile2 hieFile@HieFile{hie_module, hie_hs_file} =
  "---- HIE FILE ----" :
    PE.toS hie_hs_file :
     lookupRenderType hieFile 178:
     lookupRenderType hieFile 172:
     showModule hie_module
     <> (
      "---- INFO ----" :
      (showInfo <$> info)
     )
      <> ["---- END ----"]
  where
    info =  declarationInfo hieFile
    showInfo :: (Declaration, IdentifierDetails TypeIndex, HieAST TypeIndex) -> Text
    showInfo (decl, ids, ast) =
      "-----------------"
      <> "\n"
      <> "Declaration:\n"
      <> PE.txt (declarationStableName decl)
      <> "\nIdentifierDetails: "
      <> renderUnlabled (ppr ids)
      <> "\nAST: "
      <> showNode ast


data Declaration =
  Declaration
    { declModule :: Module
      -- ^ The module this declaration occurs in.
    , declOccName :: OccName
      -- ^ The symbol name of a declaration.
    }
  deriving
    ( Eq, Ord, Generic, NFData)

instance Show Declaration where
  show =
    declarationStableName


declarationStableName :: Declaration -> String
declarationStableName Declaration { declModule, declOccName } =
  let
    namespace
      | isVarOcc declOccName     = "var"
      | isTvOcc declOccName      = "tv"
      | isTcOcc declOccName      = "tc"
      | isDataOcc declOccName    = "data"
      | isDataSymOcc declOccName = "dataSym"
      | otherwise                = "unknown"

    in
    P.intercalate "$" [ namespace, moduleStableString declModule, "$", occNameString declOccName ]


nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <- nameModule_maybe name
  return Declaration { declModule = m, declOccName = nameOccName name }

-- | Version of findIdentifiers containing more information,
-- namely the IdentifierDetails of the declaration and the
-- node it was found in.
findIdentifiers'
  :: ( Set ContextInfo -> Bool )
  -> HieAST a
  -> Seq (Declaration, IdentifierDetails a, HieAST a)
findIdentifiers' f n@Node{ sourcedNodeInfo, nodeChildren } =
     foldMap
       (\case
           ( Left _, _ ) ->
             mempty

           ( Right name, ids@IdentifierDetails{ identInfo } ) ->
             if f identInfo then
               (, ids, n) <$> foldMap pure (nameToDeclaration name)

             else
               mempty
           )
       (foldMap (M.toList . nodeIdentifiers) (getSourcedNodeInfo sourcedNodeInfo))
  <> foldMap ( findIdentifiers' f ) nodeChildren


declarationInfo :: HieFile -> [(Declaration, IdentifierDetails TypeIndex, HieAST TypeIndex)]
declarationInfo hieFile =
  let
    HieFile{ hie_asts = HieASTs hieAsts } = hieFile
    asts = M.elems hieAsts
  in
    P.concatMap (toList . findIdentifiers' (const True)) asts


showHIEExport :: AvailInfo -> Text
showHIEExport = render "Name"

showAst :: (HiePath, HieAST TypeIndex) -> Text
showAst (path, node) =
  "Path: " <> show path
    <> "\n"
    <> showNode node

showNode :: Outputable a => HieAST a -> Text
showNode Node {sourcedNodeInfo,
                  nodeSpan,
                  nodeChildren} =
                    render "sourcedNodeInfo" sourcedNodeInfo
                    <> "\n"
                    <> render "nodeSpan" nodeSpan
                    <> "\n"
                    <> "Children: " <> show (P.length nodeChildren)
                    <> "\n"
                    <> TXT.unlines (showNode <$> nodeChildren)



showHieType :: (Outputable a, Show a) => HieType a -> Text
showHieType = \case
  HTyVarTy n -> render "Name" n
  HAppTy a (HieArgs a') -> PE.toS $ render "HAppTy" a' <> (" ~ idx: " <> show a)
  HTyConApp ifaceTyCon (HieArgs a) -> "IHTyConApp " <> showIfaceTyCon ifaceTyCon <> " ~ idx: " <> show a
  HForAllTy ((name, a), forAllTyFlag) a' -> "HForAllTy ((" <> render "Name" name
   <> " ~ idx: " <> show a <> ") " <> render "forAllTyFlag" forAllTyFlag <> ") ~ a': " <> show a'
  HFunTy a a1 a2 -> "HFunTy " <> render "a" a <> " ~ " <> render "a1" a1 <> " ~ " <> render "a2" a2
  HQualTy a a1 -> "HQualTy " <> render "a" a <> " ~ " <> render "a1" a1
  HLitTy ifaceTyLit -> "HLitTy " <> renderUnlabled (ppr ifaceTyLit)
  HCastTy a -> "HCastTy " <> render "a" a
  HCoercionTy -> "HCoercionTy"

render :: Outputable a => Text -> a -> Text
render lbl targ = PE.toS $  lbl <> ": " <> renderUnlabled (ppr targ)

renderUnlabled :: SDoc -> Text
renderUnlabled = PE.toS . renderWithContext traceSDocContext

showIfaceTyCon :: IfaceTyCon -> Text
showIfaceTyCon IfaceTyCon {ifaceTyConName, ifaceTyConInfo} = render "Name" ifaceTyConName <> " ~ " <> render "TyConInfo" ifaceTyConInfo

showModule :: Module -> [Text]
showModule  Module  {moduleName, moduleUnit} = ["Unit: " <> PE.txt moduleUnit, "Module: " <>  PE.txt moduleName]

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
      for_ hieFilePaths \hieFilePath -> do
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
      exitWith (weederExitCode w)
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

------------------- GPT ----------------------

-- Data type representing a Fixture or Hook
data Node' = Node'
  { nodeName :: String
  , nodeType :: String
  , nodeModule :: String
  , nodeDependencies :: [Node']
  } deriving (Show)

-- Main function to build the dependency tree
buildDependencyTree :: FilePath -> [FilePath] -> IO [Node']
buildDependencyTree mainHiePath otherHiePaths = do
  nameCache <- initNameCache 'z' []
  -- Read the main HieFile
  mainHieFile <- readHieFile nameCache mainHiePath

  -- Read other HieFiles
  otherHieFiles <- mapM (readHieFile nameCache) otherHiePaths

  -- Build a map of definitions from other modules
  let otherDefs = extractDefinitions $ (.hie_file_result) <$> otherHieFiles
  let defMap = M.fromList [(defName, defNode) | defNode@Node'{..} <- otherDefs, let defName = nodeName]

  -- Extract definitions from the main module
  let mainDefs = extractDefinitions [mainHieFile.hie_file_result]

  -- Build the tree
  mapM (resolveDependencies defMap) mainDefs



-- Extract definitions from HieFiles
extractDefinitions :: [HieFile] -> [Node']
extractDefinitions = P.concatMap extractFromHieFile

extractFromHieFile :: HieFile -> [Node']
extractFromHieFile HieFile{..} = P.concatMap (extractFromAST hie_module.moduleName) (getAsts hie_asts)


allNodeIdentifiers :: HieAST TypeIndex -> NodeIdentifiers TypeIndex
allNodeIdentifiers Node {sourcedNodeInfo} = M.merge  $ getIdentifiers <$> sis 
  where 
    sis = srcNodeInfos sourcedNodeInfo 
    srcNodeInfos :: SourcedNodeInfo TypeIndex -> [NodeInfo TypeIndex]
    srcNodeInfos si = M.elems si.getSourcedNodeInfo

    getIdentifiers :: NodeInfo TypeIndex -> NodeIdentifiers TypeIndex
    getIdentifiers i = i.nodeIdentifiers


extractFromAST :: ModuleName -> HieAST TypeIndex -> [Node']
extractFromAST modName ast = case M.toList (nodeIdentifiers ast) of
  [] -> P.concatMap (extractFromAST modName) (nodeChildren ast)
  ids -> [ Node'
            { nodeName = occNameString (nameOccName name)
            , nodeType = show identType
            , nodeModule = moduleNameString modName
            , nodeDependencies = []
            }
         | (Right name, identDetails) <- ids
         , let identType =  identDetails.identType
         ] ++ P.concatMap (extractFromAST modName) (nodeChildren ast)

-- Resolve dependencies for a Node
resolveDependencies :: M.Map String Node' -> Node' -> IO Node'
resolveDependencies defMap node@Node'{..} = do
  let deps = []  -- Logic to find dependencies would go here
  resolvedDeps <- mapM (lookupNode defMap) deps
  return node { nodeDependencies = resolvedDeps }

-- Lookup a Node by name
lookupNode :: M.Map String Node' -> String -> IO Node'
lookupNode defMap name = case M.lookup name defMap of
  Just node -> return node
  Nothing -> return Node'
    { nodeName = name
    , nodeType = "Unknown"
    , nodeModule = "Unknown"
    , nodeDependencies = []
    }
