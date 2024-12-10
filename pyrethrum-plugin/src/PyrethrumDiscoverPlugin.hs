module PyrethrumDiscoverPlugin (plugin) where

import Data.Either
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable

import GHC.Core.Class (Class)
import GHC.Core.InstEnv (InstEnvs, lookupInstEnv)
import GHC.Core.Predicate (isIPClass)
import GHC.Core.TyCo.Rep (PredType, Type)
import GHC.Core.TyCo.Subst
import GHC.Core.TyCon (tyConClass_maybe)
import GHC.Core.Type (splitAppTys)
import GHC.Core.Unify (tcUnifyTy)
import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.Env (hsc_home_unit, hsc_units)
import GHC.Driver.Env.Types (HscEnv (..))
import GHC.Driver.Plugins (Plugin (..), defaultPlugin, purePlugin, ParsedResult(..))
import GHC.Tc.Plugin (getTopEnv, lookupOrig, tcLookupClass, tcPluginIO)
import GHC.Tc.Solver.Monad (newWantedEq, runTcSEarlyAbort)
import GHC.Tc.Types
  ( TcPlugin (..)
  , TcPluginM
  , TcPluginSolveResult (..)
  , unsafeTcPluginTcM
  )
import GHC.Tc.Types.Constraint
  ( Ct (..)
  , CtEvidence (..)
  , DictCt (..)
  , ctPred
  , emptyRewriterSet
  )
-- import GHC.Tc.Types.CtLocEnv (CtLoc)

import GHC.Tc.Types.Evidence (EvBindsVar, Role (..))
import GHC.Tc.Utils.Env (tcGetInstEnvs)
import GHC.Tc.Utils.TcType (tcSplitTyConApp, eqType, nonDetCmpType)
import GHC.Types.Name (mkTcOcc)
import GHC.Types.Unique.FM (emptyUFM)
import GHC.Unit.Finder (FindResult (..), findPluginModule)
import GHC.Unit.Module (Module, ModuleName, mkModuleName, moduleNameString)
import GHC.Utils.Outputable (Outputable (..), showSDocUnsafe)
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.Plugins (unLoc)


type TCvSubst = Subst


plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \_opts _modSummary parsedModule ->  liftIO $ processModule parsedModule

  }

processModule :: ParsedResult -> IO ParsedResult
processModule parsedResult = do
  testModules <- findTestModulesAndTypes
  let mainCode = generateMainModule testModules
  writeMainModule mainCode
  pure parsedResult

findTestModulesAndTypes :: IO [ModuleName]
findTestModulesAndTypes = do
  -- Use GHC APIs to access the module graph and summaries
  -- Collect modules that match your test criteria
  pure [mkModuleName "Test.Module1", mkModuleName "Test.Module2"]  -- Example

generateMainModule :: [ModuleName] -> String
generateMainModule testModules =
  unlines $
    [ "{-# LANGUAGE Haskell2010 #-}"
    , "module Main where"
    ] ++
    [ "import qualified " ++ moduleNameString m ++ " as " ++ moduleNameString m | m <- testModules ] ++
    [ ""
    , "main :: IO ()"
    , "main = do"
    ] ++
    [ "  " ++ moduleNameString m ++ ".runTests" | m <- testModules ]


writeMainModule :: String -> IO ()
writeMainModule = writeFile "GeneratedMain.hs"


