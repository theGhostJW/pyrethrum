{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}

module WeederLibCopy.WeederDiscover where

import WeederLibCopy.Weeder


import Control.Monad.State.Class ( MonadState )

-- base
import Control.Applicative ( Alternative )
import Control.Monad ( guard, msum, when, unless )
import Data.Traversable ( for )
import Data.Maybe ( mapMaybe )
import Data.Foldable ( for_, traverse_, Foldable (toList) )
import Data.Function ( (&) )
import Data.List ( intercalate )
import Data.Monoid ( First( First ), getFirst )
import GHC.Generics ( Generic )
import Prelude as P hiding ( span )


-- ghc
import GHC.Data.FastString ( unpackFS )
import GHC.Types.Avail
  ( AvailInfo( Avail, AvailTC )
  , GreName( NormalGreName, FieldGreName )
  )
import GHC.Types.FieldLabel ( FieldLabel( FieldLabel, flSelector ) )
import GHC.Unit.Module ( moduleName, moduleNameString, GenModule (..) )
import GHC.Iface.Ext.Types
  ( BindType( RegularBind )
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl, ClassTyDecl, EvidenceVarBind, RecField )
  , DeclType( DataDec, ClassDec, ConDec, SynDec, FamDec )
  , EvVarSource ( EvInstBind, cls )
  , HieAST( Node, nodeChildren, nodeSpan, sourcedNodeInfo )
  , HieASTs( HieASTs, getAsts )
  , HieFile(..)
  , HieType( HTyVarTy, HAppTy, HTyConApp, HForAllTy, HFunTy, HQualTy, HLitTy, HCastTy, HCoercionTy )
  , HieArgs( HieArgs )
  , HieTypeFix( Roll )
  , IdentifierDetails( IdentifierDetails, identInfo, identType )
  , NodeAnnotation( NodeAnnotation, nodeAnnotType )
  , NodeInfo( nodeIdentifiers, nodeAnnotations )
  , Scope( ModuleScope )
  , RecFieldContext ( RecFieldOcc )
  , TypeIndex
  , getSourcedNodeInfo, hieVersion
  )

import GHC.Iface.Ext.Utils
  ( EvidenceInfo( EvidenceInfo, evidenceVar )
  , RefMap
  , findEvidenceUse
  , getEvidenceTree
  , generateReferencesMap
  , hieTypeToIface
  , recoverFullType
  )

import GHC.Iface.Type
  ( ShowForAllFlag (ShowForAllWhen)
  , pprIfaceSigmaType
  , IfaceTyCon (IfaceTyCon, ifaceTyConName)
  )

-- lens
import Control.Lens ( (%=) )

import Data.Tree (Tree)
import qualified Data.Tree as Tree

import GHC.Types.Name
  ( Name, nameModule_maybe, nameOccName
  , OccName
  , isDataOcc
  , isDataSymOcc
  , isTcOcc
  , isTvOcc
  , isVarOcc
  , occNameString
  )
import qualified Data.Set as S
import qualified Data.Map as Map
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Text as T
import Data.Text ( Text, Text, intercalate, isInfixOf )
import Debug.Trace
import PyrethrumExtras (uu, txt, toS)
import GHC.Plugins (Outputable(..), renderWithContext, defaultSDocContext)
import Text.Show.Pretty hiding (Name)
import Data.Sequence (Seq)
import qualified Data.Sequence as Sq
import System.Directory.Extra
import System.FilePath
import GHC.Types.Name.Cache
import GHC.Iface.Ext.Binary (HieFileResult(..), readHieFileWithVersion)
import System.Exit (exitFailure)
import BasePrelude (isSuffixOf)
import Control.Monad.State (execStateT)


analyseDataDeclarationDiscover :: ( Alternative m, MonadState Analysis m ) => HieFile -> RefMap TypeIndex -> HieAST TypeIndex -> m ()
analyseDataDeclarationDiscover hieFile refMap n = do
  guard $ annsContain n ("DataDecl", "TyClDecl")


  for_
    ( foldMap
        ( First . Just )
        ( findIdentifiers ( any isDataDec ) n )
    )
    \dataTypeName -> do
      define dataTypeName (nodeSpan n)

      -- Without connecting constructors to the data declaration TypeAliasGADT.hs 
      -- fails with a false positive for A
      conDecs <- for ( constructors n ) \constructor ->
        for ( foldMap ( First . Just ) ( findIdentifiers ( any isConDec ) constructor ) ) \conDec -> do
          addDependency conDec dataTypeName
          pure conDec

      -- To keep acyclicity in record declarations
      let isDependent d = Just d `elem` fmap getFirst conDecs

      for_ ( uses n ) (\d -> unless (isDependent d) $ addDependency dataTypeName d)

  for_ ( derivedInstances n ) \(d, cs, ids, ast) -> do
    define d (nodeSpan ast)

    followEvidenceUsesDiscover refMap ast d

    for_ ( uses ast ) $ addDependency d

    case identType ids of
      Just t -> for_ cs (addInstanceRootDiscover hieFile d t)
      Nothing -> pure ()

  where

    isDataDec = \case
      Decl DataDec _ -> True
      _              -> False

    isConDec = \case
      Decl ConDec _ -> True
      _             -> False

analyseClassDeclarationDiscover :: ( Alternative m, MonadState Analysis m ) =>  RefMap TypeIndex -> HieAST a -> m ()
analyseClassDeclarationDiscover refMap n@Node{ nodeSpan } = do
  guard $ annsContain n ("ClassDecl", "TyClDecl")

  for_ ( findIdentifiers isClassDeclaration n ) $ \d -> do
    define d nodeSpan

    followEvidenceUsesDiscover refMap n d

    (for_ ( findIdentifiers ( const True ) n ) . addDependency) d

  where

    isClassDeclaration =
      not . S.null . S.filter \case
        Decl ClassDec _ ->
          True

        _ ->
          False

analyseBindingDiscover :: ( Alternative m, MonadState Analysis m) => RefMap TypeIndex -> HieAST a -> m ()
analyseBindingDiscover refMap n@Node{ nodeSpan } = do
  let bindAnns = S.fromList [("FunBind", "HsBindLR"), ("PatBind", "HsBindLR")]
  guard $ any (annsContain n) bindAnns

  for_ ( findDeclarations n ) \d -> do
    define d nodeSpan

    followEvidenceUsesDiscover refMap n d

    for_ ( uses n ) $ addDependency d



analyseInstanceDeclarationDiscover :: ( Alternative m, MonadState Analysis m) => RefMap TypeIndex -> HieAST TypeIndex -> HieFile ->  m ()
analyseInstanceDeclarationDiscover refMap n@Node{ nodeSpan } hieFile = do
  guard $ annsContain n ("ClsInstD", "InstDecl")

  for_ ( findEvInstBinds n ) \(d, cs, ids, _) -> do
    -- This makes instance declarations show up in 
    -- the output if type-class-roots is set to False.
    define d nodeSpan

    followEvidenceUsesDiscover refMap n d

    for_ ( uses n ) $ addDependency d

    case identType ids of
      Just t -> for_ cs (addInstanceRootDiscover hieFile d t)
      Nothing -> pure ()


lookupPprTypeDiscover :: HieFile -> TypeIndex -> String
lookupPprTypeDiscover hieFile = renderType . lookupTypeDiscover hieFile

  where

    renderType = showSDocOneLine defaultSDocContext . pprIfaceSigmaType ShowForAllWhen . hieTypeToIface

addInstanceRootDiscover :: ( MonadState Analysis m) => HieFile -> Declaration -> TypeIndex -> Name -> m ()
addInstanceRootDiscover hieFile x t cls = do
  for_ (nameToDeclaration cls) \cls' ->
    #implicitRoots %= S.insert (InstanceRoot x cls')
  #prettyPrintedType %= Map.insert x (lookupPprTypeDiscover hieFile t)

-- | Follow evidence uses under the given node back to their instance bindings,
-- and connect the declaration to those bindings.
followEvidenceUsesDiscover :: ( MonadState Analysis m ) => RefMap TypeIndex -> HieAST a -> Declaration -> m ()
followEvidenceUsesDiscover refMap n d = do
  let getEvidenceTrees = mapMaybe (getEvidenceTree refMap)
      evidenceInfos = concatMap Tree.flatten (getEvidenceTrees names)
      instanceEvidenceInfos = evidenceInfos & filter \case
        EvidenceInfo _ _ _ (Just (EvInstBind _ _, ModuleScope, _)) -> True
        _ -> False

  for_ instanceEvidenceInfos \ev -> do
    let name = nameToDeclaration (evidenceVar ev)
    mapM_ (addDependency d) name

  where

    names = concat . Tree.flatten $ evidenceUseTree n

    evidenceUseTree :: HieAST a -> Tree [Name]
    evidenceUseTree Node{ sourcedNodeInfo, nodeChildren } = Tree.Node
      { Tree.rootLabel = concatMap (findEvidenceUse . nodeIdentifiers) (getSourcedNodeInfo sourcedNodeInfo)
      , Tree.subForest = map evidenceUseTree nodeChildren
      }

analyseStandaloneDerivingDiscover :: ( Alternative m, MonadState Analysis m) => RefMap TypeIndex -> HieAST TypeIndex -> HieFile -> m ()
analyseStandaloneDerivingDiscover rfMap ast@Node{ nodeSpan } hieFile = do
  guard $ annsContain ast ("DerivDecl", "DerivDecl")

  for_ (findEvInstBinds ast) \(d, cs, ids, _) -> do
    define d nodeSpan

    followEvidenceUsesDiscover rfMap ast d

    for_ (uses ast) $ addDependency d

    case identType ids of
      Just t -> for_ cs (addInstanceRootDiscover hieFile d t)
      Nothing -> pure ()

lookupTypeDiscover :: HieFile -> TypeIndex ->  HieTypeFix
lookupTypeDiscover hieFile t = recoverFullType t . hie_types $ hieFile

addAllDeclarationsDiscover :: ( MonadState Analysis m ) => HieAST TypeIndex -> HieFile -> m ()
addAllDeclarationsDiscover ast hieFile = do
  for_ ( findIdentifiers' ( const True ) ast )
    \(d, IdentifierDetails{ identType }, _) -> do
      addDeclaration d
      case identType of
        Just t -> do
          let hieType = lookupTypeDiscover hieFile t
              names = typeToNames hieType
          traverse_ (traverse_ (addDependency d) . nameToDeclaration) names
        Nothing -> pure ()

topLevelAnalysisDiscover :: ( MonadState Analysis m) => RefMap TypeIndex -> HieFile -> HieAST TypeIndex ->  m ()
topLevelAnalysisDiscover refMap hieFile ast@Node{ nodeChildren }  = do
  analysed <-
    runMaybeT
      ( msum
          [
            analyseStandaloneDerivingDiscover refMap ast hieFile
          , analyseInstanceDeclarationDiscover refMap ast hieFile
          , analyseBindingDiscover refMap ast
          , analyseRewriteRule ast
          , analyseClassDeclarationDiscover refMap ast
          , analyseDataDeclarationDiscover hieFile refMap ast
          , analysePatternSynonyms ast
          , analyseTypeSynonym ast
          , analyseFamilyDeclaration ast
          , analyseFamilyInstance ast
          , analyseTypeSignature ast
          ]
      )
  case analysed of
    Nothing ->
      -- We didn't find a top level declaration here, check all this nodes
      -- children.
      traverse_ (topLevelAnalysisDiscover refMap hieFile) nodeChildren

    Just () ->
      -- Top level analysis succeeded, there's nothing more to do for this node.
      return ()



-- | Incrementally update 'Analysis' with information in a 'HieFile'.
analyseHieFileDiscover :: ( MonadState Analysis m) => RefMap TypeIndex -> HieFile -> m ()
analyseHieFileDiscover refMap hieFile@HieFile{ hie_asts = HieASTs hieASTs, hie_exports, hie_module, hie_hs_file } = do
  #modulePaths %= Map.insert hie_module hie_hs_file

  for_ hieASTs \ast -> do
    addAllDeclarationsDiscover ast hieFile
    topLevelAnalysisDiscover refMap hieFile ast

  for_ hie_exports ( analyseExport hie_module )



-- | Incrementally update 'Analysis' with information in every 'HieFile'.
analyseHieFilesDiscover :: (Foldable f, MonadState Analysis m) => f HieFile -> m ()
analyseHieFilesDiscover hieFiles =
  let
    asts = concatMap (Map.elems . getAsts . hie_asts) hieFiles
    rf = generateReferencesMap asts
  in
    for_ hieFiles (analyseHieFileDiscover rf)



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



-- *********************************************************
-- *************** My Non Weeder Code **********************
-- *********************************************************

data DiscoverDeclarationPath = DiscoverDeclarationPath {
  modulePath :: Text,
  fixtureName :: Text,
  typeName :: Text
} deriving (Show, Eq, Ord)

data DiscoverFixtureSpec = FixtureSpec {
  path :: DiscoverDeclarationPath,
  parent :: Maybe DiscoverDeclarationPath
} deriving (Show, Eq, Ord)

followYourDreams :: [DiscoverFixtureSpec]
followYourDreams = uu

displayHieAst :: HieAST TypeIndex -> Text
displayHieAst ast = toS . renderWithContext defaultSDocContext $ ppr ast

data DecShow = DecShow {
  path :: Text,
  decs :: [Declaration]
} deriving Show

pPrintDisplayInfo :: DecShow -> IO ()
pPrintDisplayInfo d = do
  pPrint d.path
  pPrintList d.decs

displayInfo :: HieFile -> [DecShow]
displayInfo HieFile {hie_hs_file, hie_module = hie_module@Module {
  moduleUnit, moduleName
}, hie_types, hie_asts, hie_hs_src } =
  -- intercalate ", " $ txt <$> paths
  -- txt . ppShow $ astDs -- hangs
  decs2 -- module path
  --  str <- lookupPprType t 
 where
  asts = getAsts hie_asts
  paths = Map.keys asts
  decs = findDeclarations <$> asts
  justEg =  Map.filterWithKey (\k _ -> isInfixOf "DemoTest" $ txt k) decs
  decs2 =  Map.elems $ Map.mapWithKey (\modPath decs' -> DecShow (txt modPath) (toList decs')) decs
  -- astDs = Map.mapWithKey (\k v ->
  --    DecShow (txt k) (displayHieAst v)
  --   ) asts


demoTestFileOnly :: [String] -> [String]
demoTestFileOnly = filter (isInfixOf "DemoTest" . toS)

-- discover :: IO (ExitCode, Analysis)
discover :: IO ()
-- discover :: IO Analysis
discover =
  do
    hieFilePaths <- (traceId <$>) . P.concat <$> traverse ( getFilesIn ".hie" ) ["./."]
    hsFilePaths <- (traceId <$>) <$> getFilesIn ".hs" "./."
    nameCache <- initNameCache 'z' []

    hieFiles <-
      mapM ( readCompatibleHieFileOrExit nameCache ) $ demoTestFileOnly hieFilePaths

    let
      filteredHieFiles =
        flip filter hieFiles \hieFile -> any ( hie_hs_file hieFile `isSuffixOf`) hsFilePaths
      l = displayInfo <$> filteredHieFiles

    traverse_ (traverse_ pPrintDisplayInfo) l
    analysis <- execStateT ( analyseHieFilesDiscover filteredHieFiles ) emptyAnalysis
    uu

  -- let
  --   roots = allDeclarations analysis

  --   reachableSet =
  --     reachable
  --       analysis
  --       ( S.map DeclarationRoot roots <> filterImplicitRoots analysis ( implicitRoots analysis ) )

  --   dead =
  --     allDeclarations analysis S.\\ reachableSet

  --   warnings =
  --     Map.unionsWith (++) $
  --     foldMap
  --       ( \d ->
  --           fold $ do
  --             moduleFilePath <- Map.lookup ( declModule d ) ( modulePaths analysis )
  --             spans <- Map.lookup d ( declarationSites analysis )
  --             guard $ not $ null spans
  --             let starts = map realSrcSpanStart $ S.toList spans
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

    filterImplicitRoots :: Analysis -> S.Set Root -> S.Set Root
    filterImplicitRoots Analysis{ prettyPrintedType, modulePaths } = S.filter $ \case
      DeclarationRoot _ -> True -- keep implicit roots for rewrite rules etc

      ModuleRoot _ -> True

      InstanceRoot d c -> True



