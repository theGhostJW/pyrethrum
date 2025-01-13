-- Weeder uses head and tail
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}

module Discover where


-- glob
import qualified System.FilePath.Glob as Glob
import Data.Text as TXT
import GHC.Iface.Ext.Types (HieFile (..), hieVersion, TypeIndex, HieType (..), HieArgs (..), HiePath, HieAST (..), HieASTs (..), IdentifierDetails (..), ContextInfo (..), SourcedNodeInfo (getSourcedNodeInfo), nodeIdentifiers, HieTypeFix(..), NodeIdentifiers, NodeInfo (..), BindType (..), Scope (..), RecFieldContext (..), NodeAnnotation (NodeAnnotation), Identifier, NodeOrigin, Span)
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
import GHC (Module, ModuleName, moduleNameString, RealSrcSpan, unguardedGRHSs)
import GHC.Unit.Types (GenModule(..))
import GHC.Plugins (Outputable(..), SDoc, moduleStableString)
import GHC.Utils.Outputable (traceSDocContext, renderWithContext, showSDocOneLine, defaultSDocContext)
import GHC.Iface.Syntax (IfaceTyCon(..), pprIfaceSigmaType, ShowForAllFlag (..))
import Data.Map qualified as M
import GHC.Types.Avail (AvailInfo (..), availName)
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
import GHC.Iface.Ext.Utils (recoverFullType, hieTypeToIface, RefMap, generateReferencesMap)
import qualified Data.Set as Set
import Debug.Trace.Extended (uu, db)
import qualified Data.IntMap.Merge.Lazy as M
import Algebra.Graph  as AG (Graph, vertex, overlay, empty, edge)
import GHC.Types.SrcLoc (realSrcSpanStart, srcLocLine, srcLocCol)
import GHC.Types.SrcLoc (realSrcSpanEnd)
import GHC.Data.FastString (unpackFS)
import qualified BasePrelude as S
import PyrethrumExtras ((?))


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


refMap :: [HieFile] -> RefMap TypeIndex
refMap hieFiles = 
  generateReferencesMap asts
  where
    asts = P.concatMap (M.elems . getAsts . hie_asts) hieFiles
    -- nameHieFileMap = nameFileMap hieFiles


moduleName :: HieFile -> Text
moduleName f = PE.toS $ moduleNameString f.hie_module.moduleName

isNodeText :: Text -> Bool 
isNodeText t = ("Test" `isSuffixOf` t || "Hook" `isSuffixOf` t) && not ("_" `TXT.isPrefixOf` t)

matchFile :: HieFile -> Bool
matchFile f = isNodeText $ Discover.moduleName f

--  TODO :: moduleName' = Discover.moduleName f & PE.db' "module name"

getSuiteNodeFiles :: [HieFile] -> [HieFile]
getSuiteNodeFiles = P.filter matchFile
                                  
writeLogFile :: Text -> Text -> IO ()                            
writeLogFile fileName content = do
    TIO.writeFile (PE.toS fileName) content
    TIO.putStrLn $ "log file written: " <> fileName                            
                                    
discover :: IO Text
discover = do
  P.putStrLn "Discovering..."
  hieFiles <- getHieFiles "hie" ["./"] True
  P.putStrLn "Hie files found"
  let suiteNodeFiles = getSuiteNodeFiles hieFiles
      refMap' = refMap hieFiles
      refMapTxt = displayRefMap (nameFileMap hieFiles) refMap'

  writeLogFile "hieResultsMinimal2.log" . TXT.unlines $ P.concatMap txtHieFile2 suiteNodeFiles
  writeLogFile "refMapLogFile.log" refMapTxt
  pure "Done"

-- >>> discoverAnalysis
-- "log file written: hieResultsMinimalAnalysis.log"
discoverAnalysis :: IO Text
discoverAnalysis = do
  P.putStrLn "Discovering..."
  hieFiles <- getHieFiles "hie" ["./"] True
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
    PE.txt t
    <> ": "
    <>  PE.toS (renderType typ)
    <> "\n"
    <> "Names: "
    <> "\n"
    <> indentTxt (renderNameSet (typeToNames typ))
   where typ = lookupType hieFile t

renderNameSet  :: Set Name -> Text
renderNameSet = P.unlines . fmap (\n -> renderUnlabled (ppr n) <> " [" <> PE.txt n <> "] ") . Set.toList

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

nodeExports :: HieFile -> [Export]
nodeExports HieFile {hie_exports, hie_module, hie_hs_file} =
   (\name ->
    Export
      { name
      , typeIndex = 9999999
      , renderedType = ""
      , expModule = hie_module
      }) <$> exportNames
  where
    exportNames = P.filter (isNodeText . PE.db ("RENDERED " <>  PE.toS hie_hs_file) . renderUnlabled . ppr) $ availName <$> hie_exports

txtHieFile :: HieFile -> [Text]
txtHieFile hieFile@HieFile {
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
        ( PE.txt <$> nodeExports hieFile)
        )
      -- <> ("---- HIE SOURCE ----"
      --   : TXT.lines (decodeUtf8 hie_hs_src)
      --   )
      <> ["---- END ----"]


data Export = Export
  { name :: Name
  , typeIndex :: TypeIndex
  , renderedType :: Text
  , expModule :: Module
  } deriving (Show)


nameFileMap :: [HieFile] -> M.Map Name HieFile
nameFileMap = M.fromList . P.concatMap (\hf -> (,hf) <$> getIdentifiierExportNames  hf)

getIdentifiierExportNames :: HieFile -> [Name]
getIdentifiierExportNames f = mapMaybe availName f.hie_exports
  where
    availName = \case
      Avail name -> Just name
      AvailTC _ _ -> Nothing

displayRefMap :: Map Name HieFile -> RefMap TypeIndex -> Text
displayRefMap nameHieMap rm = P.unlines $ mapRec <$> M.toList rm
    where 
      mapRec :: (Identifier, [(Span, IdentifierDetails TypeIndex)]) -> Text
      mapRec (idn, details) = indentTxt $ renderRefMapRecord idn details

    -- P.unlines
    -- . P.filter (\i -> TXT.isInfixOf "test2" i || TXT.isInfixOf "Hook" i )
    -- . M.elems
    -- $ M.mapWithKey (renderRefMapRecord nameHieMap) rm

hieFileExportLookup :: Map Name HieFile -> Identifier -> Maybe HieFile
hieFileExportLookup nameHieMap ident =
  case ident of
    Left _name -> Nothing
    Right name ->  M.lookup name nameHieMap

-- identToTxt ::  Map Name HieFile ->  Identifier -> Text
-- identToTxt nameHieMap ident =
--   PE.toS $ either show show ident
--   <> (exportHieFile ident & maybe " - No Export Module" (\f -> " - In Module: " <> PE.toS (moduleStableString f.hie_module)))
--   where
--     exportHieFile = hieFileExportLookup nameHieMap

identToTxt :: Identifier -> Text
identToTxt ident =
  PE.toS $ either show show ident
  -- <> (exportHieFile ident & maybe " - No Export Module" (\f -> " - In Module: " <> PE.toS (moduleStableString f.hie_module)))
  -- where
  --   exportHieFile = hieFileExportLookup nameHieMap

renderRefMapRecord :: Identifier -> [(Span, IdentifierDetails TypeIndex)] -> Text
renderRefMapRecord idnt references =
  "!!!!!!!!!!!! "
   <> identToTxt idnt
   <> " !!!!!!!!!!!!"
   <> "\n"
   <> P.unlines (renderSingleRef <$> references)
  -- where
  --   hieExportFile :: Identifier -> Maybe HieFile
  --   hieExportFile = hieFileExportLookup nameHieMap

    -- renderType' :: Maybe TypeIndex -> Maybe Text
    -- renderType' mti = do
    --   ti <- mti
    --   hieFile <- hieExportFile idnt
    --   pure $ lookupRenderType hieFile ti

-- renderRefMapRecord :: Map Name HieFile -> Identifier -> [(Span, IdentifierDetails TypeIndex)] -> Text
-- renderRefMapRecord nameHieMap idnt references =
--   "!!!!!!!!!!!! "
--    <> identToTxt nameHieMap idnt
--    <> " !!!!!!!!!!!!"
--    <> "\n"
--    <> P.unlines (renderSingleRef <$> references)
--   where
--     hieExportFile :: Identifier -> Maybe HieFile
--     hieExportFile = hieFileExportLookup nameHieMap

--     renderType' :: Maybe TypeIndex -> Maybe Text
--     renderType' mti = do
--       ti <- mti
--       hieFile <- hieExportFile idnt
--       pure $ lookupRenderType hieFile ti


renderSingleRef ::  (Span, IdentifierDetails TypeIndex) -> Text
renderSingleRef (span, IdentifierDetails {identInfo, identType} ) =
  "##### identType #####\n" 
  <> PE.txt identType
  <> "\n"
  <> "##### Span #####\n" 
  <> PE.txt span
  -- <> fromMaybe (txt identType) (renderType' identType)
  <> "\n"
  <> "##### identInfo #####"
  <> "\n"
  <> renderIdentInfo identInfo
  where
  renderIdentInfo :: Set ContextInfo -> Text
  renderIdentInfo = P.unlines . fmap renderContextInfo . Set.toList

  renderContextInfo :: ContextInfo -> Text
  renderContextInfo ci = 
    render "ContextInfo" ci 
    <> "\n"
    <> "Constructor: " 
    <> showContextConstructor ci
    <> "\n"
    <> (isValBind ci  
         ? maybe "" PE.txt identType 
         $ ""
        )

  isValBind :: ContextInfo -> Bool
  isValBind = \case
      ValBind {} -> True
      _ -> False

  showContextConstructor :: ContextInfo -> Text
  showContextConstructor = \case 
      Use -> "Use"             
      MatchBind -> "MatchBind"
      IEThing {} -> "IEThing"
      TyDecl -> "TyDecl"
      ValBind {} -> "ValBind"
      PatternBind {} -> "PatternBind"
      ClassTyDecl {} -> "ClassTyDecl"
      Decl {} -> "Decl"
      TyVarBind {} -> "TyVarBind"
      RecField {} -> "RecField"
      EvidenceVarBind {} -> "EvidenceVarBind"
      EvidenceVarUse -> "EvidenceVarUse"


-- $> discover
-- "log file written: hieResultsMinimal2.log"
txtHieFile2 :: HieFile -> [Text]
txtHieFile2 hieFile@HieFile{hie_module, hie_hs_file, hie_types} =
  "---- HIE FILE ----" :
     PE.toS hie_hs_file :
    --  "test2" :
    --  lookupRenderType hieFile 321:
    --  "infoThreadHook" :
    --  lookupRenderType hieFile 284:
     showModule hie_module
    --  <> (
    --   "---- INFO ----" :
    --   P.filter (not . TXT.null) (showInfo <$> info)
    --  )
    --   <> ["\n"]
     <>
      ("---- HIE EXPORTS ----" :
        ( PE.txt <$> nodeExports hieFile)
        )
    <> (
      "---- TYPES ----" :
      (showHieType <$> toList hie_types)
     )
     <> ["\n"]
     <> (
      "---- INFO II ----" :
      P.filter (not . TXT.null) info2
     )
      <> ["\n"]
      <> ["---- END ----"]
  where
    info =  declarationInfo hieFile
    info2 =  declarationInfo2 hieFile
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


indentTxt :: Text -> Text
indentTxt = TXT.unlines . fmap ("  "  <>) . TXT.lines

declarationInfo2 :: HieFile -> [Text]
declarationInfo2 hieFile =
  hieAstTxt <$> kvs

  where
    HieFile{ hie_asts = HieASTs hieAsts } = hieFile
    keys = M.keys hieAsts
    kvs = M.toList hieAsts
    vals = M.elems hieAsts

    hieAstTxt :: (HiePath, HieAST TypeIndex) -> Text
    hieAstTxt  (hiePath, hieAst) =
        "hiePath: "
        <> PE.txt hiePath
        <> "\n"
        <> astToTxt hieAst
       where
        astToTxt :: HieAST TypeIndex -> Text
        astToTxt Node {sourcedNodeInfo, nodeSpan , nodeChildren} =
         indentTxt $ 
         PE.txt nodeSpan
         <> "\n"
         <> TXT.unlines (nodeInfoTxt <$> M.elems sourcedNodeInfo.getSourcedNodeInfo)
         <> "children:"
         <> "\n"
          <> indentTxt (TXT.unlines $ astToTxt <$> nodeChildren)

    nodeInfoTxt :: NodeInfo TypeIndex -> Text
    nodeInfoTxt NodeInfo {nodeAnnotations, nodeType, nodeIdentifiers} =
      indentTxt $
        "\n"
        <> renderNodeidentifiers nodeIdentifiers
        <> renderNodeType nodeType
        <> TXT.unlines (render "node annotation" <$> S.toList nodeAnnotations)

    renderNodeType :: [TypeIndex] -> Text
    renderNodeType typs = "nodeType:" <> (P.null typs ? " NA\n" $ "\n" <> TXT.unlines (lookupRenderType hieFile <$> typs))
     
    renderNodeidentifiers :: NodeIdentifiers TypeIndex -> Text
    renderNodeidentifiers nodeIdentifiers = 
      "Node Identifiers: " <>
       TXT.unlines (nodeIdentifierText <$> M.toList nodeIdentifiers )   

    nodeIdentifierText :: (Identifier, IdentifierDetails TypeIndex) -> Text
    nodeIdentifierText (idnt, details) = 
      getName idnt 
      <> "\n"
      <> "Identifier Details"
      <> "\n"
      <> indentTxt (identDetailsText details)
      where 
        getName = either (renderUnlabled . ppr) (renderUnlabled . ppr)

    identDetailsText :: IdentifierDetails TypeIndex -> Text
    identDetailsText IdentifierDetails {identType, identInfo} = 
      "type: " <>
      fromMaybe "No Type" identTypeText <> 
      "\n" <> 
      "info\n" <> 
       identInfoText
      where
        identTypeText = lookupRenderType hieFile <$> identType
        identInfoText = renderUnlabled (ppr identInfo)
    --   where 
    --     uu
        -- in
        --   "Node Info: " <> PE.txt nodeInfoMap
        --   <> "\n"
        --   <> "Node Span: " <> PE.txt nodeSpan
        --   <> "\n"
        --   <> "Children: " <> PE.txt (P.length nodeChildren)
        --   <> "\n"
        --   <> "Node Children: " <> TXT.unlines (hieAstTxt <$> nodeChildren)
  -- in
    -- P.concatMap (toList . findIdentifiers' (const True)) asts

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
