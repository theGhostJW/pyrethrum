-- Weeder uses head and tail
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}

module Discover where


-- glob
import qualified System.FilePath.Glob as Glob
import Data.Text as TXT
import GHC.Iface.Ext.Types (HieFile (..), hieVersion, TypeIndex, HieType (..), HieArgs (..), HiePath, HieAST (..), HieASTs (..), IdentifierDetails (..), ContextInfo (..), SourcedNodeInfo (getSourcedNodeInfo), nodeIdentifiers, HieTypeFix(..), NodeIdentifiers, NodeInfo (..), BindType (..), Scope (..), RecFieldContext (..), NodeAnnotation (NodeAnnotation))
import GHC.Types.Name.Cache
import GHC.Iface.Ext.Binary (HieFileResult(..), readHieFileWithVersion, readHieFile)
import BasePrelude (
 ExitCode (..),
 throwIO,
 hPutStrLn,
 Handler (..),
 catches,
 writeChan, 
 newChan, 
 getChanContents)
import BasePrelude qualified as BP
import Prelude as P
import qualified Data.List
import System.FilePath (isExtSeparator)
import Control.Concurrent.Async (async, ExceptionInLinkedThread (..), link)
-- import Text.Show.Pretty (pPrint)
import PyrethrumExtras qualified as PE
-- import PyrethrumExtras.Test qualified as PET
import GHC (Module, ModuleName, moduleNameString, RealSrcSpan)
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
import Debug.Trace.Extended (uu, db)
import qualified Data.IntMap.Merge.Lazy as M
import Algebra.Graph  as AG (Graph, vertex, overlay, empty, edge)
import GHC.Types.SrcLoc (realSrcSpanStart, srcLocLine, srcLocCol)
import GHC.Types.SrcLoc (realSrcSpanEnd)
import GHC.Data.FastString (unpackFS)


{-
- list modules ending in Test
- or Hook
- extract Fixtures and depended on hooks
- extract hook dependencies 
- validate??
- generate main
-}

-- >>> gptDiscover
gptDiscover :: IO ()
gptDiscover = do
  hieFiles <- hieFilePaths "hie" ["./"]
  P.putStrLn "Hie files found"
  let mainHiePath = BP.head $ P.filter (\f ->
                                   let
                                    moduleName = PE.txt f
                                   in
                                    "Minimal" `isInfixOf` moduleName || "mBase" `isInfixOf` moduleName
                                     )  hieFiles
  let otherHiePaths = P.filter (\f ->
                                   let
                                    moduleName = PE.txt f
                                   in
                                     "mBase" `isInfixOf` moduleName ||
                                     "DemoTest" `isInfixOf` moduleName
                                     )  hieFiles
  tree <- buildDependencyTree mainHiePath otherHiePaths
  let
   logFile = "hieResultsMinimalGPT.log"
   message = "log file written: " <> logFile
  TIO.writeFile (PE.toS logFile) (TXT.unlines $ PE.txt <$> tree)
  putStrLn message


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


      logFile = "hieResultsMinimal2.log"
      fileContent = P.concatMap txtHieFile2 filesOfInterest

      -- logFile = "hieResultsMinimal.log"
      -- fileContent = P.concatMap txtHieFile filesOfInterest

      message = "log file written: " <> logFile
  TIO.writeFile (PE.toS logFile) (TXT.unlines fileContent)
  TIO.putStrLn message
  pure message

-- >>> discoverAnalysis
-- "log file written: hieResultsMinimalAnalysis.log"
discoverAnalysis :: IO Text
discoverAnalysis = do
  P.putStrLn "Discovering..."
  hieFiles <- getHieFiles "hie" ["./"] True
  P.putStrLn "Hie files found"
  -- let filesOfInterest = P.filter (\f ->
  --                                  let
  --                                   moduleName = PE.txt f.hie_module.moduleName
  --                                  in
  --                                   "Minimal" `isInfixOf` moduleName
  --                                   || "mBase" `isInfixOf` moduleName
  --                                    )  hieFiles
  let filesOfInterest = hieFiles


      logFile = "hieResultsMinimalAnalysis.log"
      asts = do 
        hieFile <- filesOfInterest
        M.elems hieFile.hie_asts.getAsts
      fileContent = P.foldl' analyseBinding emptyAnalysis asts

      -- logFile = "hieResultsMinimal.log"
      -- fileContent = P.concatMap txtHieFile filesOfInterest

      message = "log file written: " <> logFile
  TIO.writeFile (PE.toS logFile) (PE.txt fileContent)
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
  , hie_hs_src
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

-- >>> discover
-- "log file written: hieResultsMinimal2.log"
txtHieFile2 :: HieFile -> [Text]
txtHieFile2 hieFile@HieFile{hie_module, hie_hs_file} =
  "---- HIE FILE ----" :
    PE.toS hie_hs_file :
     "test2" :
     lookupRenderType hieFile 321:
     "infoThreadHook" :
     lookupRenderType hieFile 284:
     showModule hie_module
     <> (
      "---- INFO ----" :
      P.filter (not . TXT.null) (showInfo <$> info)
     )
      <> ["---- END ----"]
  where
    info =  declarationInfo hieFile
    showInfo :: (Declaration, IdentifierDetails TypeIndex, HieAST TypeIndex) -> Text
    showInfo (decl, ids, ast) =
      wantInfo PE.?
        "-----------------"
        <> "\n"
        <> "!!!! Declaration !!!!\n"
        <> stableName
        <> "\n!!!! IdentifierDetails !!!!\n"
        <> renderUnlabled (ppr ids)
        <> "\n!!!! AST !!!!\n"
        <> showNode ast
       $ ""
      where
        stableName = PE.toS $ declarationStableName decl
        wantInfo = "test2" `isInfixOf` stableName || "infoThreadHook" `isInfixOf` stableName
          -- "infoThreadHook" `isInfixOf` stableName || "addOnceIntHook" `isInfixOf` stableName


findDeclarations :: HieAST a -> Seq Declaration
findDeclarations =
  findIdentifiers
    (   not
      . Set.null
      . Set.filter \case
          -- Things that count as declarations
          ValBind RegularBind ModuleScope _ -> True
          PatternBind ModuleScope _ _       -> True
          Decl _ _                          -> True
          TyDecl                            -> True
          ClassTyDecl{}                     -> True

          -- Anything else is not a declaration
          _ -> False
    )



findIdentifiers
  :: ( Set ContextInfo -> Bool )
  -> HieAST a
  -> Seq Declaration
findIdentifiers f = fmap (\(d, _, _) -> d) . findIdentifiers' f


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
    

unNodeAnnotation :: NodeAnnotation -> (String, String)
unNodeAnnotation (NodeAnnotation x y) = (unpackFS x, unpackFS y)
   
annsContain :: HieAST a -> (String, String) -> Bool
annsContain Node{ sourcedNodeInfo } ann =
  P.any (Set.member ann . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

-- | A root for reachability analysis.
data Root
  = -- | A given declaration is a root.
    DeclarationRoot Declaration
  | -- | We store extra information for instances in order to be able
    -- to specify e.g. all instances of a class as roots.
    InstanceRoot 
      Declaration -- ^ Declaration of the instance
      Declaration -- ^ Declaration of the parent class
  | -- | All exported declarations in a module are roots.
    ModuleRoot Module
  deriving
    ( Eq, Ord, Generic, NFData, Show )

instance Show Module where
  show :: Module -> String
  show = moduleStableString

instance Show Name where
  show = occNameString . nameOccName

-- | All information maintained by 'analyseHieFile'.
data Analysis =
  Analysis
    { dependencyGraph :: Graph Declaration
      -- ^ A graph between declarations, capturing dependencies.
    , declarationSites :: Map Declaration (Set (Int, Int))
      -- ^ A partial mapping between declarations and their line numbers.
      -- This Map is partial as we don't always know where a Declaration was
      -- defined (e.g., it may come from a package without source code).
      -- We capture a set of sites, because a declaration may be defined in
      -- multiple locations, e.g., a type signature for a function separate
      -- from its definition.
    , implicitRoots :: Set Root
      -- ^ Stores information on Declarations that may be automatically marked
      -- as always reachable. This is used, for example, to capture knowledge 
      -- not yet modelled in weeder, or to mark all instances of a class as 
      -- roots.
    , exports :: Map Module ( Set Declaration )
      -- ^ All exports for a given module.
    , modulePaths :: Map Module FilePath
      -- ^ A map from modules to the file path to the .hs file defining them.
    , prettyPrintedType :: Map Declaration String
      -- ^ Used to match against the types of instances and to replace the
      -- appearance of declarations in the output
    , requestedEvidence :: Map Declaration (Set Name)
      -- ^ Map from declarations to the names containing evidence uses that
      -- should be followed and treated as dependencies of the declaration.
      -- We use this to be able to delay analysing evidence uses until later,
      -- allowing us to begin the rest of the analysis before we have read all
      -- hie files.
    }
  deriving
    ( Generic, NFData, Show )


isUse :: ContextInfo -> Bool
isUse = \case
  Use -> True
  -- not RecFieldMatch and RecFieldDecl because they occur under
  -- data declarations, which we do not want to add as dependencies
  -- because that would make the graph no longer acyclic
  -- RecFieldAssign will be most likely accompanied by the constructor
  RecField RecFieldOcc _ -> True
  _ -> False


uses :: HieAST a -> Set Declaration
uses =
    foldMap Set.singleton
  . findIdentifiers (P.any isUse)


define :: Declaration -> RealSrcSpan -> Analysis -> Analysis
define decl span' analysis@Analysis{declarationSites, dependencyGraph} =
  if realSrcSpanStart span' == realSrcSpanEnd span' then
    analysis
  else
    let loc = ( srcLocLine (realSrcSpanStart span'),  srcLocCol (realSrcSpanStart span'))
    in
      analysis
        { 
          declarationSites = M.insertWith Set.union decl ( Set.singleton loc) declarationSites  ,
          dependencyGraph = overlay ( vertex decl ) dependencyGraph 
        }
  
  
  -- when ( realSrcSpanStart span /= realSrcSpanEnd span ) do
-- | The empty analysis - the result of analysing zero @.hie@ files.
emptyAnalysis :: Analysis
emptyAnalysis = Analysis AG.empty mempty mempty mempty mempty mempty mempty


-- | @addDependency x y@ adds the information that @x@ depends on @y@.
addDependency :: Declaration -> Analysis -> Declaration -> Analysis
addDependency  x analysis y =
  analysis { dependencyGraph = overlay ( edge x y ) analysis.dependencyGraph }


analyseBinding ::  Analysis -> HieAST a -> Analysis
analyseBinding analysis ast@Node{ nodeSpan } = 
  let 
    bindAnns = Set.fromList [("FunBind", "HsBindLR"), ("PatBind", "HsBindLR")]
    declarations = findDeclarations ast
  in
  if P.any (annsContain ast) bindAnns then
    P.foldl' (\acc decl -> 
      let result = define decl nodeSpan acc
      in 
         P.foldl' (addDependency decl) result ( uses ast ) 
      ) analysis declarations

  else
    analysis


  -- for_ ( findDeclarations ast ) \d -> do
  --   define d nodeSpan

  --   requestEvidence ast d

  --   for_ ( uses ast ) $ addDependency d

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
  "Path: " <> P.show path
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
                    <> "Children: " <> P.show (P.length nodeChildren)
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

hieFilePaths :: String -> [FilePath] -> IO [FilePath]
hieFilePaths hieExt hieDirectories =  P.concat <$>
      traverse ( getFilesIn hiePat )
        ( if BP.null hieDirectories
          then ["./."]
          else hieDirectories
        )
      where
        hiePat = "**/*." <> hieExtNoSep
        hieExtNoSep = if isExtSeparator (Data.List.head hieExt) then Data.List.tail hieExt else hieExt

-- | Find and read all .hie files in the given directories according to the given parameters,
-- exiting if any are incompatible with the current version of GHC.
-- The .hie files are returned as a lazy stream in the form of a list.
--
-- Will rethrow exceptions as 'ExceptionInLinkedThread' to the calling thread.
getHieFiles :: String -> [FilePath] -> Bool -> IO [HieFile]
getHieFiles hieExt hieDirectories requireHsFiles = do

  hieFilePaths' <- hieFilePaths hieExt hieDirectories

  hsFilePaths :: [FilePath] <-
    if requireHsFiles
      then getFilesIn "**/*.hs" "./."
      else pure []

  hieFileResultsChan <- newChan

  nameCache <-
    initNameCache 'z' []

  a <- async $ handleWeederException do
    readHieFiles nameCache hieFilePaths' hieFileResultsChan hsFilePaths
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
  , nodeType :: Text
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
extractFromHieFile hieFile@HieFile{..} = P.concatMap (extractFromAST hieFile hie_module.moduleName) (getAsts hie_asts)

allNodeIdentifiers :: HieAST TypeIndex -> NodeIdentifiers TypeIndex
allNodeIdentifiers Node {sourcedNodeInfo} = M.unions allNis
  where
    sis = srcNodeInfos sourcedNodeInfo
    allNis =  getIdentifiers <$> sis
    srcNodeInfos :: SourcedNodeInfo TypeIndex -> [NodeInfo TypeIndex]
    srcNodeInfos si = M.elems si.getSourcedNodeInfo

    getIdentifiers :: NodeInfo TypeIndex -> NodeIdentifiers TypeIndex
    getIdentifiers i = i.nodeIdentifiers


extractFromAST :: HieFile -> ModuleName -> HieAST TypeIndex -> [Node']
extractFromAST hieFile modName ast = case M.toList (allNodeIdentifiers ast) of
  [] -> P.concatMap (extractFromAST hieFile modName) (nodeChildren ast)
  ids -> [ Node'
            { nodeName = occNameString (nameOccName name)
            , nodeType = identType & maybe "Unknown" (lookupRenderType hieFile)
            , nodeModule = moduleNameString modName
            , nodeDependencies = []
            }
         | (Right name, identDetails) <- ids
         , let identType =  identDetails.identType
         ] ++ P.concatMap (extractFromAST hieFile modName) (nodeChildren ast)

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
