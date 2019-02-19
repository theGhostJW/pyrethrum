{-# LANGUAGE QuasiQuotes #-}

module AuxFiles where

import Foundation.Extended as F
import qualified Prelude as P
import Paths_pyrethrum
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import qualified System.IO as S
import qualified Data.Char as C
import Data.Time.LocalTime

binDir :: IO AbsDir
binDir = parseAbsDir =<< getBinDir

auxBase :: IO (Either P.IOError AbsDir)
auxBase = subDirFromBaseDir binDir [reldir|auxFiles|]

subPath :: IO (Either P.IOError AbsDir) -> Path Rel a -> IO (Either P.IOError (Path Abs a))
subPath prent chld = ((</> chld) <$>) <$> prent

auxDir :: RelDir -> IO (Either P.IOError AbsDir)
auxDir = subPath auxBase

-- local temp not OS
tempDir :: IO (Either P.IOError AbsDir)
tempDir = auxDir [reldir|temp|]

logDir :: IO (Either P.IOError AbsDir)
logDir = auxDir [reldir|logs|]

dataDir :: IO (Either P.IOError AbsDir)
dataDir = auxDir [reldir|data|]

tempFile :: RelFile -> IO (Either P.IOError AbsFile)
tempFile = subPath tempDir

logFile :: RelFile -> IO (Either P.IOError AbsFile)
logFile = subPath logDir

dataFile :: RelFile -> IO (Either P.IOError AbsFile)
dataFile = subPath logDir

_tempFile = tempFile [relfile|demoTemp.txt|]

newtype FileExt = FileExt {unFileExt :: String} deriving (Show, Eq, Ord)

logFileExt :: FileExt
logFileExt = FileExt ".log"

-- based on https://gist.github.com/jdeseno/9501557
base36 :: Integer -> Int -> String
base36 num minWidth =
  let 
    conv :: Int -> P.String -> P.String
    conv n s = C.chr (n + 48 + ((n-9) `div` (-27) * (-7))) : s

    units :: Integer -> [Int]
    units n 
      | n < (36 :: Integer) = [fromIntegral n] 
      | otherwise = units (n `div` (36 :: Integer)) <> [fromIntegral n `P.rem` 36]

    unpadded :: P.String
    unpadded = foldr conv [] $ units num

    len :: Int
    len = fromCount $ length unpadded

    prefix :: P.String
    prefix = fromIntegral len < minWidth ? replicate (toCount (minWidth - len)) '0' $ []
  in
    toS $ prefix <> foldr conv [] (units num)

logFileSuffix :: String -> FileExt -> String
logFileSuffix suffix fileExt = "_" <> suffix <> unFileExt fileExt

fullLogFileName :: String -> String -> FileExt -> String
fullLogFileName prefix suffix fileExt = prefix <> logFileSuffix suffix fileExt

logFilePrefix :: ZonedTime -> String
logFilePrefix now =
  let
    msLeftInYear :: Integer
    msLeftInYear =  let
                      utcNow = zonedTimeToUTC now
                      (y, m, d)  = toGregorian $ utctDay utcNow
                      nyd = UTCTime (fromGregorian (y + 1) 1 1) 0
                      daysDif = diffDays (utctDay nyd) (utctDay utcNow)
                      msPerDay = 24 * 60 * 60 * 1000
                      timeDifms = P.round $ fromIntegral (diffTimeToPicoseconds $ utctDayTime nyd P.- utctDayTime utcNow) / 1000000000
                    in
                      daysDif * msPerDay + timeDifms
  in 
    base36 msLeftInYear 7 <> "_" <> toS (formatTime defaultTimeLocale (toCharList "%F_%H-%M-%S") now)

logFilePath :: Maybe String -> String -> FileExt -> IO (Either P.IOError (String, AbsFile))
logFilePath mNamePrefix suffix fileExt = 
  do
    pfx <- maybef mNamePrefix
              (logFilePrefix <$> getZonedTime)
              pure
    relPath <- parseRelFileSafe $ fullLogFileName pfx suffix fileExt
    eitherf relPath
      (pure . Left . P.userError . toS . show)
      (\relFle -> ((pfx,) <$>) <$> logFile relFle)

safeOpenFile :: AbsFile -> S.IOMode -> IO (Either P.IOError S.Handle)
safeOpenFile pth mode = 
  catchIOError (Right <$> S.openFile (toFilePath pth) mode) (pure . Left)

data HandleInfo = HandleInfo {
  prefix :: String,
  path ::  AbsFile,
  fileHandle :: S.Handle
}

logFileHandle ::  Maybe String -> String -> FileExt -> IO (Either P.IOError HandleInfo)
logFileHandle mFilePrefix fileNameSuffix fileExt = do
                                                    ethPth <- logFilePath mFilePrefix fileNameSuffix fileExt
                                                    eitherf ethPth 
                                                      (pure . Left)
                                                      (\(pfx, pth) -> (HandleInfo pfx pth <$>) <$> safeOpenFile pth S.WriteMode)