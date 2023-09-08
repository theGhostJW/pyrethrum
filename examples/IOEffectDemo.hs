module IOEffectDemo where

import PyrethrumExtras as PE
import Chronos (Time, now)
import DSL.FileSystemEffect
import Effectful
import DSL.Out
import DSL.Internal.ApEvent
import Effectful.Error.Static as E (Error, runError)
import qualified Data.Text as T
import BasePrelude (openFile, hClose, hGetContents)
import DSL.FileSystemIOInterpreter
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

--  TODO: pyrelude depricate debug in favour of trace

-- use eff
listFileImp :: (FileSystem :> es, Out ApEvent :> es) => Eff es [Text]
listFileImp = do
  log "listFileImp"
  files <- walkDirAccum Nothing (\root subs files -> pure files) [absdir|C:\Pyrethrum|]
  log "done"
  pure . filter ("cabal" `T.isInfixOf`) $ toS . toFilePath <$> files

apEventOut :: forall a es. (IOE :> es) => Eff (Out ApEvent : es) a -> Eff es a
apEventOut = runOut print

ioRun :: Eff '[FileSystem, Out ApEvent, Error FSException, IOE] a -> IO (Either (CallStack, FSException) a)
ioRun = runEff . runError . apEventOut . runFileSystem

logShow :: (Out ApEvent :> es, Show a) => a -> Eff es ()
logShow = out . User . Log . txt

log :: (Out ApEvent :> es) => Text -> Eff es ()
log = out . User . Log

-- $> ioRun effDemo
effDemo :: Eff '[FileSystem, Out ApEvent, Error FSException, IOE] ()
effDemo = do
  res <- listFileImp
  chk res
 where
  chk _ = log "This is a effDemo"

-- $> ioRun effDemo2
effDemo2 :: Eff '[FileSystem, Out ApEvent, Error FSException, IOE] ()
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
- file related functions: read file - see polysemy version and src\DSL\FileSystemPsy.hs note in docs on hover and beware of readfile
- adapt effectful: effectful\src\Effectful\FileSystem\IO.hs
  - src\DSL\FileSystemPsy.hs
- find lazy API and test from within effect

-}
