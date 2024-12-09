module Plugin where


module TestDiscoverPlugin (plugin) where

import GhcPlugins
import HsSyn
import TcRnTypes
import TcRnMonad
import Control.Monad

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \_opts _modSummary parsedModule -> do
      newParsedModule <- liftIO $ processModule parsedModule
      pure newParsedModule
  }

processModule :: ParsedResult -> IO ParsedResult
processModule parsedResult = do
  let hsModule = unLoc $ prParsedSource parsedResult
  -- Analyze the module to find test modules and types
  testModules <- findTestModulesAndTypes
  -- Generate or modify the Main module
  let mainModule = generateMainModule testModules
  -- Return the modified parsed result
  pure parsedResult

findTestModulesAndTypes :: IO [ModuleName]
findTestModulesAndTypes = do
  -- Use GHC APIs to access the module graph and summaries
  -- Collect modules that match your test criteria
  pure [ModuleName "Test.Module1", ModuleName "Test.Module2"]  -- Example

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
writeMainModule code = writeFile "GeneratedMain.hs" code


