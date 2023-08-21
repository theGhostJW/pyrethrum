{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}

module WeederLibCopy.WeederDiscover where

import WeederLibCopy.Weeder


import Control.Monad.State.Class ( MonadState )

-- base
import Control.Applicative ( Alternative )
import Control.Monad ( guard, msum, when, unless )
import Data.Traversable ( for )
import Data.Maybe ( mapMaybe )
import Data.Foldable ( for_, traverse_ )
import Data.Function ( (&) )
import Data.List ( intercalate )
import Data.Monoid ( First( First ), getFirst )
import GHC.Generics ( Generic )
import Prelude hiding ( span )


-- ghc
import GHC.Data.FastString ( unpackFS )
import GHC.Types.Avail
  ( AvailInfo( Avail, AvailTC )
  , GreName( NormalGreName, FieldGreName )
  )
import GHC.Types.FieldLabel ( FieldLabel( FieldLabel, flSelector ) )
import GHC.Iface.Ext.Types
  ( BindType( RegularBind )
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl, ClassTyDecl, EvidenceVarBind, RecField )
  , DeclType( DataDec, ClassDec, ConDec, SynDec, FamDec )
  , EvVarSource ( EvInstBind, cls )
  , HieAST( Node, nodeChildren, nodeSpan, sourcedNodeInfo )
  , HieASTs( HieASTs, getAsts )
  , HieFile( HieFile, hie_asts, hie_exports, hie_module, hie_hs_file, hie_types )
  , HieType( HTyVarTy, HAppTy, HTyConApp, HForAllTy, HFunTy, HQualTy, HLitTy, HCastTy, HCoercionTy )
  , HieArgs( HieArgs )
  , HieTypeFix( Roll )
  , IdentifierDetails( IdentifierDetails, identInfo, identType )
  , NodeAnnotation( NodeAnnotation, nodeAnnotType )
  , NodeInfo( nodeIdentifiers, nodeAnnotations )
  , Scope( ModuleScope )
  , RecFieldContext ( RecFieldOcc )
  , TypeIndex
  , getSourcedNodeInfo
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
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.Trans.Maybe (MaybeT(..))


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
      not . Set.null . Set.filter \case
        Decl ClassDec _ ->
          True

        _ ->
          False

analyseBindingDiscover :: ( Alternative m, MonadState Analysis m) => RefMap TypeIndex -> HieAST a -> m ()
analyseBindingDiscover refMap n@Node{ nodeSpan } = do
  let bindAnns = Set.fromList [("FunBind", "HsBindLR"), ("PatBind", "HsBindLR")]
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
    #implicitRoots %= Set.insert (InstanceRoot x cls')
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



