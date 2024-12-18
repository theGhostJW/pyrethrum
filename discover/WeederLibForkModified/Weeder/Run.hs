{-# language RecordWildCards #-}
{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module WeederLibForkModified.Weeder.Run ( runWeeder, Weed(..), formatWeed ) where

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
  , moduleNameString
  )
import GHC.Iface.Ext.Types ( HieFile( hie_asts ), getAsts )
import GHC.Iface.Ext.Utils (generateReferencesMap)

-- parallel
import Control.Parallel (pseq)
import Control.Parallel.Strategies (parMap, rdeepseq)

-- regex-tdfa
import Text.Regex.TDFA ( matchTest, Regex )

-- transformers

-- weeder
import WeederLibForkModified.Weeder
import WeederLibForkModified.Weeder.Config as CFG
import PyrethrumExtras (db)

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

-- | Run Weeder on the given .hie files with the given 'Config'.
--
-- Returns a list of 'Weed's that can be displayed using
-- 'formatWeed', and the final 'Analysis'.
runWeeder :: CFG.Config -> [HieFile] -> ([Weed], Analysis)
runWeeder weederConfig@Config{ rootPatterns, typeClassRoots, rootInstances, rootModules } hieFiles =
  let 
    asts = concatMap (Map.elems . getAsts . hie_asts) hieFiles

    rf = generateReferencesMap asts

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
