module IOEffectDemo where

import BasePrelude (openFile)
import qualified DSL.FileSystemDocInterpreter as DII
import DSL.FileSystemEffect
import qualified DSL.FileSystemIOInterpreter as IOI
import DSL.Internal.ApEvent
import DSL.Out
import Data.List.Extra (isInfixOf)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Error.Static (Error, runError)
import Path (parseAbsFile, reldir, relfile, toFilePath)
import PyrethrumExtras (Abs, File, Path, bracket, parseRelFileSafe, toS, txt, uu, (?), MonadMask)
import System.IO (hClose, hGetContents)

-- https://www.tweag.io/blog/2017-07-27-streaming-programs/

headLine :: Text -> Text
headLine = unlines . take 1 . lines

printHeadLine :: String -> IO ()
printHeadLine = putStrLn . toS . headLine . toS

-- import Control.Exception (bracket)
-- import System.IO (hGetContents, hClose, openFile, IOMode(ReadMode))

-- $> printHeadLineFail1 "pyrethrum.cabal"
printHeadLineFail1 :: FilePath -> IO ()
printHeadLineFail1 path = do
  contents <- bracket (openFile path ReadMode) hClose hGetContents
  printHeadLine contents
-- illegal operation (delayed read on closed handle)

-- $> printHeadLineWorks "pyrethrum.cabal"
printHeadLineWorks :: FilePath -> IO ()
printHeadLineWorks path =
  bracket (openFile path ReadMode) hClose (hGetContents >=> printHeadLine)
-- cabal-version: 3.6

-- $> printHeadLineForce "pyrethrum.cabal" 
printHeadLineForce :: FilePath -> IO ()
printHeadLineForce path = do
    str <- bracket (openFile path ReadMode) hClose \h -> do
      contents <- hGetContents h
      pure $!! contents
    printHeadLine str
-- cabal-version: 3.6

forceBracket :: (MonadMask m, NFData b) => m a
 -> (a -> m c)
 -> (a -> m b)
 -> m b
forceBracket aquire cleanUp action = bracket aquire cleanUp (action >=> (pure $!!))

-- $> printHeadLineForceBracket "pyrethrum.cabal" 
printHeadLineForceBracket :: FilePath -> IO ()
printHeadLineForceBracket path = do
  contents <- forceBracket (openFile path ReadMode) hClose hGetContents
  printHeadLine contents
  -- cabal-version: 3.6

--  print . headLine $ toS c

-- >  illegal operation (delayed read on closed handle)

-- -- todo - use a more believable base fuunction
-- demo :: forall es. (Out ApEvent :> es, FileSystem :> es) => Eff es ()
-- demo = do
--   paths <- getPaths
--   chk paths
--  where
--   chk :: [Path Abs File] -> Eff es ()
--   chk _ = log "This is a check"

-- -- $> runDemo
-- runDemo :: IO (Either (CallStack, DII.DocException) ())
-- runDemo = docRun demo

-- demo2 :: forall es. (Out ApEvent :> es, FileSystem :> es) => Eff es ()
-- demo2 = do
--   paths <- getPaths
--   chk paths
--  where
--   chk :: [Path Abs File] -> Eff es ()
--   chk ps = log $ length ps > 0 ? "More than 0 paths" $ "zero paths"

-- -- $> runDemo2
-- runDemo2 :: IO (Either (CallStack, DII.DocException) ())
-- runDemo2 = docRun demo2

-- -- uses
-- demo3 :: forall es. (Out ApEvent :> es, FileSystem :> es) => Eff es ()
-- demo3 = do
--   paths <- getPathsData
--   chk paths
--  where
--   chk :: a -> Eff es ()
--   chk _ = log "This is a check demo 3"

-- -- $> runDemo3
-- runDemo3 :: IO (Either (CallStack, DII.DocException) ())
-- runDemo3 = docRun demo3

-- docRunHandleAll :: Eff '[FileSystem, Out ApEvent, Error DII.DocException, IOE] a -> IO (Either (CallStack, DII.DocException) a)
-- docRunHandleAll = runEff . runError . apEventOut . DII.runFileSystem

-- docRun :: Eff '[FileSystem, Out ApEvent, Error DII.DocException, IOE] a -> IO (Either (CallStack, DII.DocException) a)
-- docRun = runEff . runError . apEventOut . DII.runFileSystem

-- apEventOut :: forall a es. (IOE :> es) => Eff (Out ApEvent : es) a -> Eff es a
-- apEventOut = runOut print

-- log :: (Out ApEvent :> es) => Text -> Eff es ()
-- log = out . Log

-- getPaths :: (Out ApEvent :> es, FileSystem :> es) => Eff es [Path Abs File]
-- getPaths = do
--   s <- findFilesWith isDeleteMe [[reldir|chris|]] [relfile|foo.txt|]
--   r <- test s
--   log $ r ? "yes" $ "no"
--   pure s
--  where
--   isDeleteMe :: Path Abs File -> Eff es Bool
--   isDeleteMe = pure . isInfixOf "deleteMe" . toFilePath

-- data PathResult = PathResult
--   { title :: Text
--   , paths :: [Path Abs File]
--   }

-- --  proves we are OK with strict data
-- getPathsData :: (Out ApEvent :> es, FileSystem :> es) => Eff es PathResult
-- getPathsData = do
--   p <- findFilesWith isDeleteMe [[reldir|chris|]] [relfile|foo.txt|]
--   r <- test p
--   log $ r ? "yes" $ "no"
--   pure $
--     PathResult
--       { title = "Hi"
--       , paths = p
--       }
--  where
--   isDeleteMe :: Path Abs File -> Eff es Bool
--   isDeleteMe = pure . isInfixOf "deleteMe" . toFilePath

-- test :: [Path Abs File] -> Eff es Bool
-- test _ = pure True

-- -- TODO => hide string based prntLn et. al.
-- consoleSink :: (Show a) => Sink a
-- consoleSink = Sink print