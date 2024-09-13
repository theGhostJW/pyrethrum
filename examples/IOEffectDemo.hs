module IOEffectDemo where

import PyrethrumExtras as PE
import Chronos (Time, now)
import DSL.FileSystemEffect
    ( walkDirAccum )
import Effectful ( IOE, type (:>), Eff, runEff )
import DSL.Out
import DSL.Internal.NodeEvent
import Data.Text qualified as T
import BasePrelude (openFile, hClose, hGetContents)
import DSL.FileSystemIOInterpreter ( FileSystem, runFileSystem )
import System.Time.Extra (sleep)
import PyrethrumExtras.IO (putTxt)


{-
\************************************************************
\************ standard haskell laziness examples ************
\************************************************************
-- https://www.tweag.io/blog/2017-07-27-streaming-programs/
-}

headLine :: Text -> Text
headLine = T.unlines . take 1 . T.lines

printHeadLine :: String -> IO ()
printHeadLine = putText . headLine . toS

-- import Control.Exception (bracket)
-- import System.IO (hGetContents, hClose, openFile, IOMode(ReadMode))

-- $ > printHeadLineFail1 "pyrethrum.cabal"
printHeadLineFail1 :: FilePath -> IO ()
printHeadLineFail1 path = do
  contents <- bracket (openFile path ReadMode) hClose hGetContents
  printHeadLine contents

-- illegal operation (delayed read on closed handle)

-- $ > printHeadLineWorks "pyrethrum.cabal"
printHeadLineWorks :: FilePath -> IO ()
printHeadLineWorks path =
  bracket (openFile path ReadMode) hClose (hGetContents >=> printHeadLine)

-- cabal-version: 3.6

-- $ > printHeadLineForce "pyrethrum.cabal"
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

-- $ > printHeadLineForceBracket "pyrethrum.cabal"
printHeadLineForceBracket :: FilePath -> IO ()
printHeadLineForceBracket path = do
  contents <- forceBracket (openFile path ReadMode) hClose hGetContents
  printHeadLine contents

-- cabal-version: 3.6

printTime :: Text -> Time -> IO ()
printTime msg t = putTxt $ msg <> " " <> toS (show t)

-- $ > timeTest
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
listFileImp :: (FileSystem :> es, Out NodeEvent :> es) => Eff es [Text]
listFileImp = do
  log "listFileImp"
  files <- walkDirAccum Nothing (\_root _subs files -> pure files) [absdir|/pyrethrum/pyrethrum|]
  log "done"
  pure . filter ("cabal" `T.isInfixOf`) $ toS . toFilePath <$> files


apEventOut :: forall a es. (IOE :> es) => Eff (Out NodeEvent : es) a -> Eff es a
apEventOut = runOut print

ioRun :: Eff '[FileSystem, Out NodeEvent,  IOE] a -> IO  a
ioRun ap = ap & 
  runFileSystem & 
  apEventOut & 
  runEff  

logShow :: (Out NodeEvent :> es, Show a) => a -> Eff es ()
logShow = out . User . Log . txt

log :: (Out NodeEvent :> es) => Text -> Eff es ()
log = out . User . Log

-- $ > ioRun effDemo
effDemo :: Eff '[FileSystem, Out NodeEvent, IOE] ()
effDemo = do
  res <- listFileImp
  chk res
 where
  chk _ = log "This is a effDemo"

-- $ > ioRun effDemo2
effDemo2 :: Eff '[FileSystem, Out NodeEvent, IOE] ()
effDemo2 = do
  res <- listFileImp
  chk res
 where
  chk res = do
    traverse_ log res
    log $ length res > 5 ? "its BIG" $ "its small"

-- log $ T.unlines res

{-
TODO:
- file related functions: read file - see polyssemy version and src\DSL\FileSystemPsy.hs note in docs on hover and beware of readfile
- adapt effectful: effectful\src\Effectful\FileSystem\IO.hs
  - src\DSL\FileSystemPsy.hs
- find lazy API and test from within effect
- make file functions relative to working dir

-}
