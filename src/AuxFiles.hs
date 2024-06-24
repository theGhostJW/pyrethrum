module AuxFiles where

import BasePrelude as P (IOError, getExecutablePath, userError)
import Chronos as C (
  Date (dateYear),
  Datetime (datetimeDate),
  DatetimeFormat (DatetimeFormat),
  SubsecondPrecision (SubsecondPrecisionAuto),
  Time,
  Timespan (getTimespan),
  Year (getYear),
  encode_YmdHMS,
  now,
  timeFromYmdhms,
  timeToDatetime,
  width,
  (...),
 )
import Control.Exception (throw)
import Data.Char qualified as C
import Data.Text.IO (putStrLn, writeFile)
import Path.Extended (
  Abs,
  AbsDir,
  AbsFile,
  Path,
  Rel,
  RelDir,
  RelFile,
  parent,
  parseAbsFile,
  parseRelFileSafe,
  reldir,
  relfile,
  toFilePath,
  (</>),
 )
import PyrethrumExtras (
  catchIOError,
  toS,
  txt,
  (?),
 )
import PyrethrumExtras.IO as PIO (subDirFromBaseDir, openFile)
import System.IO qualified as S
import Prelude hiding (putStrLn, writeFile)

data WantConsole = Console | NoConsole deriving (Eq)

-- TODO: Revisit add to efffect must work on all OSes?
-- TODO: base on working dir including validation that working dir is correct instead of current dir

auxBase :: IO AbsDir -> IO (Either IOError AbsDir)
auxBase projRoot = subDirFromBaseDir projRoot [reldir|auxFiles|]

subPath :: IO (Either IOError AbsDir) -> Path Rel a -> IO (Either IOError (Path Abs a))
subPath prent chld = ((</> chld) <$>) <$> prent

auxDir :: IO AbsDir -> RelDir -> IO (Either IOError AbsDir)
auxDir projRoot = subPath (auxBase projRoot)

-- local temp not OS
tempDirBase :: IO AbsDir -> IO (Either IOError AbsDir)
tempDirBase projRoot = auxDir projRoot [reldir|temp|]

logDirBase :: IO AbsDir -> IO (Either IOError AbsDir)
logDirBase projRoot = auxDir projRoot [reldir|logs|]

dataDirBase :: IO AbsDir -> IO (Either IOError AbsDir)
dataDirBase projRoot = auxDir projRoot [reldir|data|]

tempFileBase :: IO AbsDir -> RelFile -> IO (Either IOError AbsFile)
tempFileBase projRoot = subPath (tempDirBase projRoot)

logFile :: IO AbsDir -> RelFile -> IO (Either IOError AbsFile)
logFile projRoot = subPath (logDirBase projRoot)

dataFile :: IO AbsDir -> RelFile -> IO (Either IOError AbsFile)
dataFile projRoot = subPath (dataDirBase projRoot)

dumpTxt :: IO AbsDir -> Text -> RelFile -> IO ()
dumpTxt projRoot txt' file = do
  ePth <- tempFileBase projRoot file
  ePth
    & either
      throw
      (\p -> writeFile (toFilePath p) txt')

_tempFile :: IO (Either IOError AbsFile)
_tempFile = tempFileBase (parent <$> (parseAbsFile =<< getExecutablePath)) [relfile|demoTemp.txt|]

newtype FileExt = FileExt {unFileExt :: Text} deriving (Show, Eq, Ord)

logFileExt :: FileExt
logFileExt = FileExt ".log"

-- based on https://gist.github.com/jdeseno/9501557
base36 :: Integer -> Int -> Text
base36 num minWidth =
  let
    conv :: Int -> String -> String
    conv n s = C.chr (n + 48 + ((n - 9) `div` (-27) * (-7))) : s

    units :: Integer -> [Int]
    units n
      | n < (36 :: Integer) = [fromIntegral n]
      | otherwise = units (n `div` (36 :: Integer)) <> [fromIntegral n `rem` 36]

    unpadded :: String
    unpadded = foldr conv [] $ units num

    len :: Int
    len = length unpadded

    prefix :: String
    prefix = fromIntegral len < minWidth ? replicate (minWidth - len) '0' $ []
   in
    toS $ prefix <> foldr conv [] (units num)

logFilePrefix :: Time -> Text
logFilePrefix currentTime =
  let
    nextYear = (getYear . dateYear . datetimeDate . timeToDatetime $ currentTime) + 1
    nyd = timeFromYmdhms nextYear 1 1 0 0 0
    msLeftInYear :: Integer
    msLeftInYear = fromIntegral (getTimespan . width $ nyd ... currentTime) `div` 1000000
   in
    base36 msLeftInYear 7 <> "_" <> toS (encode_YmdHMS SubsecondPrecisionAuto (DatetimeFormat (Just '_') (Just '_') (Just '-')) (timeToDatetime currentTime))

logFilePath :: IO AbsDir -> Maybe Text -> Text -> FileExt -> IO (Either IOError (Text, AbsFile))
logFilePath projRoot mNamePrefix suffix fileExt =
  do
    pfx <-
      mNamePrefix
        & maybe
          (logFilePrefix <$> now)
          pure
    relPath <- parseRelFileSafe $ pfx <> "_" <> suffix <> fileExt.unFileExt
    relPath
      & either
        (pure . Left . userError . toS . show)
        (fmap ((pfx,) <$>) . logFile projRoot)

data HandleInfo = HandleInfo
  { prefix :: Text
  , path :: AbsFile
  , fileHandle :: S.Handle
  }

logFileHandle :: IO AbsDir -> Text -> FileExt -> IO (Either IOError HandleInfo)
logFileHandle projRoot fileNameSuffix fileExt =
  logFilePath projRoot Nothing fileNameSuffix fileExt
    >>= either
      (pure . Left)
      (\(pfx, pth) -> (HandleInfo pfx pth <$>) <$> openFile pth S.WriteMode)

-- Writing temp files used mostly used for de bugging
defaultTempFileName :: RelFile
defaultTempFileName = [relfile|temp.txt|]

listToText :: (Show a) => [a] -> Text
listToText lst = unlines $ txt <$> lst

toTempBase' :: IO AbsDir -> WantConsole -> Text -> RelFile -> IO ()
toTempBase' projRoot wantConsole txt' fileName =
  do
    ethQPth <- tempFileBase projRoot fileName
    ethQPth
      & either
        (\e -> putStrLn $ "failed to generate path: " <> txt e)
        ( \adsPath ->
            do
              let
                destPath = toFilePath adsPath
              when
                (wantConsole == Console)
                (putStrLn $ "temp file written to: " <> toS destPath)
              writeFile destPath txt'
        )

toTemp' :: IO AbsDir -> Text -> RelFile -> IO ()
toTemp' projRoot = toTempBase' projRoot Console

toTemp :: IO AbsDir -> Text -> IO ()
toTemp projRoot = flip (toTemp' projRoot) defaultTempFileName

toTempFromList' :: (Show a) => IO AbsDir -> [a] -> RelFile -> IO ()
toTempFromList' projRoot = toTemp' projRoot . listToText

toTempFromList :: (Show a) => IO AbsDir -> [a] -> IO ()
toTempFromList projRoot = flip (toTempFromList' projRoot) defaultTempFileName

toTempNoConsole' :: IO AbsDir -> Text -> RelFile -> IO ()
toTempNoConsole' projRoot = toTempBase' projRoot NoConsole

toTempNoConsole :: IO AbsDir -> Text -> IO ()
toTempNoConsole projRoot = flip (toTempNoConsole' projRoot) defaultTempFileName

toTempFromListNoConsole' :: (Show a) => IO AbsDir -> [a] -> RelFile -> IO ()
toTempFromListNoConsole' projRoot = toTempNoConsole' projRoot . listToText

toTempFromListNoConsole :: (Show a) => IO AbsDir -> [a] -> IO ()
toTempFromListNoConsole projRoot = flip (toTempFromList' projRoot) defaultTempFileName
