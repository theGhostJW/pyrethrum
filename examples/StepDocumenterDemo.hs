module StepDocumenterDemo where

import DSL.FileSystemDocInterpreter qualified as DII
import DSL.FileSystemEffect (
  FileSystem,
  findFilesWith,
  walkDirAccum,
 )
import DSL.Internal.NodeEvent (NodeEvent (User), ULog (Log))
import DSL.Out (Out, Sink (Sink), out, runOut)
import Data.List.Extra (isInfixOf)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Error.Static (Error, runError)
import Path (absdir, reldir, relfile, toFilePath, Abs, File, Path)
import PyrethrumExtras ((?))

type FSOut es = (Out NodeEvent :> es, FileSystem :> es)

-- todo - use a more believable base function
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

demo2 :: forall es. (Out NodeEvent :> es, FileSystem :> es) => Eff es ()
demo2 = do
  paths <- getPaths
  chk paths
 where
  chk :: [Path Abs File] -> Eff es ()
  chk ps = log $ length ps > 0 ? "More than 0 paths" $ "zero paths"

-- $ > runDemo2
runDemo2 :: IO (Either (CallStack, DII.DocException) ())
runDemo2 = docRun demo2

-- uses
demo3 :: forall es. (Out NodeEvent :> es, FileSystem :> es) => Eff es ()
demo3 = do
  paths <- getPathsData
  chk paths
 where
  chk :: a -> Eff es ()
  chk _ = log "This is a check demo 3"

-- $ > runDemo3
runDemo3 :: IO (Either (CallStack, DII.DocException) ())
runDemo3 = docRun demo3

docRunHandleAll :: Eff '[FileSystem, Out NodeEvent, Error DII.DocException, IOE] a -> IO (Either (CallStack, DII.DocException) a)
docRunHandleAll = runEff . runError . apEventOut . DII.runFileSystem

docRun :: Eff '[FileSystem, Out NodeEvent, Error DII.DocException, IOE] a -> IO (Either (CallStack, DII.DocException) a)
docRun = runEff . runError . apEventOut . DII.runFileSystem

apEventOut :: forall a es. (IOE :> es) => Eff (Out NodeEvent : es) a -> Eff es a
apEventOut = runOut print

log :: (Out NodeEvent :> es) => Text -> Eff es ()
log = out . User . Log

getPaths :: (Out NodeEvent :> es, FileSystem :> es) => Eff es [Path Abs File]
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
getPathsData :: (Out NodeEvent :> es, FileSystem :> es) => Eff es PathResult
getPathsData = do
  p <- findFilesWith isDeleteMe [[reldir|chris|]] [relfile|foo.txt|]
  output <- walkDirAccum Nothing (\_root _subs files -> pure files) [absdir|/pyrethrum/pyrethrum|]
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