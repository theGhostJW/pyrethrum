{-# language RecordWildCards #-}
{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WeederLibForkModified.Weeder.Run ( runWeeder, Weed(..), formatWeed,
  matchSuffix,
  isTestName,
  isHookName,
  filterRule,
  showModuleName
   ) where

-- base
import Control.Applicative ( liftA2 )
import Control.Monad ( guard )
import Data.List ( sortOn )
import Data.Foldable ( fold, foldl' )
import Data.Function ( (&) )

-- containers
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- ghc
import GHC.Plugins
  ( occNameString
  , unitString
  , moduleUnit
  , moduleName
  , moduleNameString, Module
  )
import GHC.Iface.Ext.Types ( HieFile(.. ), getAsts, TypeIndex, Identifier, Span, IdentifierDetails (..), ContextInfo (..), HieTypeFix (..), HieType (..), HieArgs (..), RecFieldContext (..) )
import GHC.Iface.Ext.Utils (generateReferencesMap, RefMap, recoverFullType, hieTypeToIface)

-- parallel
import Control.Parallel (pseq)
import Control.Parallel.Strategies (parMap, rdeepseq)

-- regex-tdfa
import Text.Regex.TDFA ( matchTest, Regex )

-- transformers

-- weeder
import WeederLibForkModified.Weeder
import WeederLibForkModified.Weeder.Config as CFG
import PyrethrumExtras (db, dbf, txt)
import Prelude hiding (show)
import GHC.Show (Show(..))
import GHC.Utils.Outputable (SDoc, renderWithContext, traceSDocContext, Outputable (..), showSDocOneLine)
import qualified PyrethrumExtras as PE
import qualified Data.Text as T
import qualified Algebra.Graph.Export as Text
import GHC.Plugins (defaultSDocContext)
import GHC.Iface.Type (pprIfaceSigmaType)
import GHC.Iface.Syntax (ShowForAllFlag(..))
import GHC.Types.Name (Name, getOccString, NamedThing)
import GHC.Iface.Syntax (IfaceTyCon(..))
import Control.Lens.Internal.Zoom (May(May))
import GHC.Types.Avail (AvailInfo(..))
import GHC.Unit (moduleStableString)




matchSuffix :: [Text] -> Text -> Bool
matchSuffix suffixes name = any (`T.isSuffixOf` name) suffixes


isTestName :: Text -> Bool
isTestName name = matchSuffix ["Test", "Tests"] name && not ("_" `T.isPrefixOf` name)

isHookName :: Text -> Bool
isHookName name = matchSuffix ["Hook", "Hooks"] name && not ("_" `T.isPrefixOf` name)

filterRule :: Text -> Bool
filterRule name = isTestName name || isHookName name

showModuleName :: Module -> Text
showModuleName = PE.toS . moduleNameString . moduleName


data Weed = Weed
  { weedPackage :: String
  , weedPath :: FilePath
  , weedLine :: Int
  , weedCol :: Int
  , weedDeclaration :: Declaration
  , weedPrettyPrintedType :: Maybe String
  }

formatWeed :: Weed -> String
formatWeed Weed{..} =
  weedPackage <> ": " <> weedPath <> ":" <> show weedLine <> ":" <> show weedCol <> ": "
    <> case weedPrettyPrintedType of
      Nothing -> occNameString weedDeclaration.declOccName
      Just t -> "(Instance) :: " <> t


hieFileExportLookup :: Map Name HieFile -> Identifier -> Maybe HieFile
hieFileExportLookup nameHieMap ident =
  case ident of
    Left _name -> Nothing
    Right name ->  Map.lookup name nameHieMap

identToTxt ::  Map Name HieFile ->  Identifier -> Text
identToTxt nameHieMap ident =
  PE.toS $ either show show ident
  <> (exportHieFile ident & maybe " - No Export Module" (\f -> " - In Module: " <> PE.toS (moduleStableString f.hie_module)))
  where
    exportHieFile = hieFileExportLookup nameHieMap

render :: Outputable a => Text -> a -> Text
render lbl targ = PE.toS $  lbl <> ": " <> renderUnlabled (ppr targ)


lookupType :: HieFile -> TypeIndex -> HieTypeFix
lookupType HieFile {hie_types}  t = recoverFullType t hie_types

renderType :: HieTypeFix -> String
renderType = showSDocOneLine defaultSDocContext . pprIfaceSigmaType ShowForAllWhen . hieTypeToIface

renderNameSet  :: Set Name -> Text
renderNameSet = unlines . fmap (renderUnlabled .  ppr) . Set.toList


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
    hieArgsTypes = foldMap (typeToNames . snd) . filter fst

lookupRenderType :: HieFile -> TypeIndex -> Text
lookupRenderType hieFile t =
    PE.toS (renderType typ)
    <> "\n"
    <> "Names"
    <> "\n"
    <> renderNameSet (typeToNames typ)
   where typ = lookupType hieFile t


nameFileMap :: [HieFile] -> Map.Map Name HieFile
nameFileMap = Map.fromList . concatMap (\hf -> (,hf) <$> getIdentifiierExportNames  hf)

getIdentifiierExportNames :: HieFile -> [Name]
getIdentifiierExportNames f = mapMaybe availName f.hie_exports
  where
    availName = \case
      Avail name -> Just name
      AvailTC _ _ -> Nothing

renderRefMapRecord ::  Map Name HieFile -> Identifier -> [(Span, IdentifierDetails TypeIndex)] -> Text
renderRefMapRecord nameHieMap idnt references =
  "!!!!!!!!!!!! "
   <> identToTxt nameHieMap idnt
   <> " !!!!!!!!!!!!"
   <> "\n"
   <> unlines (renderSingleRef <$> references)
  where
    hieExportFile :: Identifier -> Maybe HieFile
    hieExportFile = hieFileExportLookup nameHieMap

    renderType' :: Maybe TypeIndex -> Maybe Text
    renderType' mti = do
      ti <- mti
      hieFile <- hieExportFile idnt
      pure $ lookupRenderType hieFile ti


    renderSingleRef ::  (Span, IdentifierDetails TypeIndex) -> Text
    renderSingleRef (_span, IdentifierDetails {identInfo, identType} ) =
      "##### identType #####\n" 
      <> txt identType
      -- <> fromMaybe (txt identType) (renderType' identType)
      <> "\n"
      <> "##### identInfo #####"
      <> "\n"
      <> renderIdentInfo identInfo
     where
      renderIdentInfo :: Set ContextInfo -> Text
      renderIdentInfo = unlines . fmap renderContextInfo . Set.toList

      renderContextInfo :: ContextInfo -> Text
      renderContextInfo ci = 
        render "ContextInfo" ci 
        <> "\n"
        <> "Constructor: " 
        <> showContextConstructor ci
        <> "\n"
        <> (if isValBind ci then fromMaybe (txt identType) (renderType' identType) else "")

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

renderUnlabled :: Outputable a => a -> Text
renderUnlabled = PE.toS . renderWithContext traceSDocContext . ppr

isUse :: ContextInfo -> Bool
isUse = \case
  Use -> True
  -- not RecFieldMatch and RecFieldDecl because they occur under
  -- data declarations, which we do not want to add as dependencies
  -- because that would make the graph no longer acyclic
  -- RecFieldAssign will be most likely accompanied by the constructor
  RecField RecFieldOcc _ -> True
  _ -> False

displayRefMap ::  Map Name HieFile -> RefMap TypeIndex -> Text
displayRefMap nameHieMap rm =
    unlines
    . filter (\i -> T.isInfixOf "test2" i || T.isInfixOf "Hook" i )
    . Map.elems
    $ Map.mapWithKey (renderRefMapRecord nameHieMap) rm

-- | Run Weeder on the given .hie files with the given 'Config'.
--
-- Returns a list of 'Weed's that can be displayed using
-- 'formatWeed', and the final 'Analysis'.
runWeeder :: CFG.Config -> [HieFile] -> ([Weed], Analysis)
runWeeder weederConfig@Config{ rootPatterns, typeClassRoots, rootInstances, rootModules } hieFiles =
  let
    asts = concatMap (Map.elems . getAsts . hie_asts) hieFiles
    nameHieFileMap = nameFileMap hieFiles


    rf = dbf "REFERENCE MAP" (displayRefMap nameHieFileMap) $ generateReferencesMap asts

    analyses =
      parMap rdeepseq (\hf -> execState (analyseHieFile weederConfig hf) emptyAnalysis) hieFiles

    analyseEvidenceUses' =
      if typeClassRoots
        then id
        else analyseEvidenceUses rf

    analysis1 =
      foldl' mappend mempty analyses

    -- Evaluating 'analysis1' first allows us to begin analysis 
    -- while hieFiles is still being read (since rf depends on all hie files)
    analysis = analysis1 `pseq`
     analyseEvidenceUses' analysis1 -- & db "analysis"

    -- We limit ourselves to outputable declarations only rather than all
    -- declarations in the graph. This has a slight performance benefit,
    -- at the cost of having to assume that a non-outputable declaration
    -- will always either be an implicit root or irrelevant.
    roots =
      Set.filter
        ( \d ->
            any
              (`matchTest` displayDeclaration d)
              rootPatterns
        )
        ( outputableDeclarations analysis )

    matchingModules =
      Set.filter
        ((\s -> any (`matchTest` s) rootModules) . moduleNameString . moduleName)
      ( Map.keysSet analysis.exports )

    reachableSet =
      reachable
        analysis
        ( Set.map DeclarationRoot roots
        <> Set.map ModuleRoot matchingModules
        <> filterImplicitRoots analysis analysis.implicitRoots
        )

    -- We only care about dead declarations if they have a span assigned,
    -- since they don't show up in the output otherwise
    dead =
      outputableDeclarations analysis Set.\\ reachableSet

    warnings =
      Map.unionsWith (++) $
      foldMap
        ( \d ->
            fold $ do
              moduleFilePath <- Map.lookup d.declModule  analysis.modulePaths
              let packageName = unitString . moduleUnit $ d.declModule
              starts <- Map.lookup d analysis.declarationSites
              let locs = (,) packageName <$> Set.toList starts
              guard $ not $ null starts
              return [ Map.singleton moduleFilePath ( liftA2 (,) locs (pure d) ) ]
        )
        dead

    weeds =
      Map.toList warnings & concatMap \( weedPath, declarations ) ->
        sortOn fst declarations & map \( (weedPackage, (weedLine, weedCol)) , weedDeclaration ) ->
          Weed { weedPrettyPrintedType = Map.lookup weedDeclaration analysis.prettyPrintedType
               , weedPackage
               , weedPath
               , weedLine
               , weedCol
               , weedDeclaration
               }

  in (weeds, analysis)

  where

    filterImplicitRoots :: Analysis -> Set Root -> Set Root
    filterImplicitRoots Analysis{ prettyPrintedType, modulePaths } = Set.filter $ \case
      DeclarationRoot _ -> True -- keep implicit roots for rewrite rules etc

      ModuleRoot _ -> True

      InstanceRoot d c -> typeClassRoots || matchingType
        where
          matchingType :: Bool
          matchingType =
            let mt = Map.lookup d prettyPrintedType
                matches = maybe (const False) (flip matchTest) mt
            in any (maybe True matches) filteredInstances

          filteredInstances :: [Maybe Regex]
          filteredInstances =
            map (.instancePattern)
            . filter (maybe True (`matchTest` displayDeclaration c) . (.classPattern))
            . filter (maybe True modulePathMatches . (.modulePattern))
            $ rootInstances


          modulePathMatches p = maybe False (p `matchTest`) (Map.lookup d.declModule modulePaths)


displayDeclaration :: Declaration -> String
displayDeclaration d =
  moduleNameString ( moduleName d.declModule ) <> "." <> occNameString d.declOccName
