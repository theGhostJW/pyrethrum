module StepDocumenterDemo where

import qualified DSL.FileSystemDocInterpreter as DII
import DSL.FileSystemEffect (
  FileSystem,
  findFilesWith,
  walkDirAccum,
 )
import qualified DSL.FileSystemIOInterpreter as IOI
import DSL.Internal.ApEvent (ApEvent (User), ULog (Log))
import DSL.Out (Out, Sink (Sink), out, runOut)
import Data.List.Extra (isInfixOf)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Error.Static (Error, runError)
import Path (absdir, parseAbsFile, reldir, relfile, toFilePath)
import PyrethrumExtras (Abs, File, Path, parseRelFileSafe, toS, txt, uu, (?))

type FSOut es = (Out ApEvent :> es, FileSystem :> es)

-- todo - use a more believable base fuunction
demo :: forall es. (FSOut es) => Eff es ()
demo = do
  paths <- getPaths
  chk paths
 where
  chk :: [Path Abs File] -> Eff es ()
  chk _ = log "This is a check"

-- $ > runDemo
runDemo :: IO (Either (CallStack, DII.DocException) ())
runDemo = docRun demo

demo2 :: forall es. (Out ApEvent :> es, FileSystem :> es) => Eff es ()
demo2 = do
  paths <- getPaths
  chk paths
 where
  chk :: [Path Abs File] -> Eff es ()
  chk ps = log $ length ps > 0 ? "More than 0 paths" $ "zero paths"

-- $> runDemo2
runDemo2 :: IO (Either (CallStack, DII.DocException) ())
runDemo2 = docRun demo2

-- uses
demo3 :: forall es. (Out ApEvent :> es, FileSystem :> es) => Eff es ()
demo3 = do
  paths <- getPathsData
  chk paths
 where
  chk :: a -> Eff es ()
  chk _ = log "This is a check demo 3"

-- $> runDemo3
runDemo3 :: IO (Either (CallStack, DII.DocException) ())
runDemo3 = docRun demo3

docRunHandleAll :: Eff '[FileSystem, Out ApEvent, Error DII.DocException, IOE] a -> IO (Either (CallStack, DII.DocException) a)
docRunHandleAll = runEff . runError . apEventOut . DII.runFileSystem

docRun :: Eff '[FileSystem, Out ApEvent, Error DII.DocException, IOE] a -> IO (Either (CallStack, DII.DocException) a)
docRun = runEff . runError . apEventOut . DII.runFileSystem

apEventOut :: forall a es. (IOE :> es) => Eff (Out ApEvent : es) a -> Eff es a
apEventOut = runOut print

log :: (Out ApEvent :> es) => Text -> Eff es ()
log = out . User . Log

getPaths :: (Out ApEvent :> es, FileSystem :> es) => Eff es [Path Abs File]
getPaths = do
  s <- findFilesWith isDeleteMe [[reldir|chris|]] [relfile|foo.txt|]
  r <- test s
  log $ r ? "yes" $ "no"
  pure s
 where
  isDeleteMe :: Path Abs File -> Eff es Bool
  isDeleteMe = pure . isInfixOf "deleteMe" . toFilePath

data PathResult = PathResult
  { title :: Text
  , moreOutput :: [Path Abs File]
  , paths :: [Path Abs File]
  }

--  proves we are OK with strict data
getPathsData :: (Out ApEvent :> es, FileSystem :> es) => Eff es PathResult
getPathsData = do
  p <- findFilesWith isDeleteMe [[reldir|chris|]] [relfile|foo.txt|]
  output <- walkDirAccum Nothing (\root subs files -> pure files) [absdir|C:\Pyrethrum|]
  r <- test p
  log $ r ? "yes" $ "no"
  pure
    $ PathResult
      { title = "Hi"
      , moreOutput = output
      , paths = p
      }
 where
  isDeleteMe :: Path Abs File -> Eff es Bool
  isDeleteMe = pure . isInfixOf "deleteMe" . toFilePath

test :: [Path Abs File] -> Eff es Bool
test _ignored = pure True

-- TODO => hide string based prntLn et. al.
consoleSink :: (Show a) => Sink a
consoleSink = Sink print