module IOEffectDemo where

import BasePrelude (openFile)
import qualified DSL.FileSystemDocInterpreter as DII
import DSL.FileSystemEffect
import qualified DSL.FileSystemIOInterpreter as IOI
import DSL.Internal.ApEvent
import DSL.Out

import Chronos (Time, now)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Error.Static (Error, runError)
import Path (parseAbsFile, reldir, relfile, toFilePath, absdir)
import PyrethrumExtras (Abs, ConvertString, File, MonadMask, Path, bracket, parseRelFileSafe, toS, txt, uu, (?))
import System.IO (hClose, hGetContents)

-- TODO: add to pyrelude

import qualified DSL.FileSystemEffect as IOI
import Data.Text as T (concat, isInfixOf)
import System.Time.Extra (sleep)

{-
\************************************************************
\************ standard haskell laziness examples ************
\************************************************************
-- https://www.tweag.io/blog/2017-07-27-streaming-programs/
-}
putTxt :: (ConvertString a String) => a -> IO ()
putTxt = putStrLn . toS

headLine :: Text -> Text
headLine = unlines . take 1 . lines

printHeadLine :: String -> IO ()
printHeadLine = putTxt . headLine . toS

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

forceBracket ::
  (MonadMask m, NFData b) =>
  m a ->
  (a -> m c) ->
  (a -> m b) ->
  m b
forceBracket aquire cleanUp action = bracket aquire cleanUp (action >=> (pure $!!))

-- $> printHeadLineForceBracket "pyrethrum.cabal"
printHeadLineForceBracket :: FilePath -> IO ()
printHeadLineForceBracket path = do
  contents <- forceBracket (openFile path ReadMode) hClose hGetContents
  printHeadLine contents

-- cabal-version: 3.6

printTime :: Text -> Time -> IO ()
printTime msg t = putTxt $ msg <> " " <> toS (show t)

-- $> timeTest
timeTest :: IO ()
timeTest = do
  t <- now
  printTime "time 1" t
  sleep 1
  t2 <- now
  -- printTime "time 2" t2
  sleep 1.1
  t3 <- now
  printTime "time 3" t3
  sleep 1
  putTxt "summary"
  printTime "time 1" t
  printTime "time 2" t2
  printTime "time 3" t3

-- not out of order as expected => api must be strict
-- time 1 Time {getTime = 1689018956967000000}
-- time 3 Time {getTime = 1689018959089000000}
-- summary
-- time 1 Time {getTime = 1689018956967000000}
-- time 2 Time {getTime = 1689018957979000000}
-- time 3 Time {getTime = 1689018959089000000}

-- use eff
listFileImp :: (FileSystem :> es) => Eff es [Text]
listFileImp = do
  files <- walkDirAccum Nothing (\root subs files -> pure files) [absdir|C:\Pyrethrum|]
  pure . filter ("cabal" `isInfixOf`) $ toS . toFilePath <$> files

apEventOut :: forall a es. (IOE :> es) => Eff (Out ApEvent : es) a -> Eff es a
apEventOut = runOut print

ioRun :: Eff '[FileSystem, Out ApEvent, Error IOI.FSException, IOE] a -> IO (Either (CallStack, IOI.FSException) a)
ioRun = runEff . runError . apEventOut . IOI.runFileSystem

log :: (Out ApEvent :> es, Show a) => a -> Eff es ()
log = out . Log . txt

-- $> ioRun effDemo
effDemo :: Eff '[FileSystem, Out ApEvent, Error IOI.FSException, IOE] ()
effDemo = do
  res <- listFileImp
  log $ T.concat res

{-
TODO:
- file related functions: read file - see polysemy version and src\DSL\FileSystemPsy.hs note in docs on hover and beware of readfile
- adapt effectful: effectful\src\Effectful\FileSystem\IO.hs
  - src\DSL\FileSystemPsy.hs
- find lazy API and test from within effect

-}
