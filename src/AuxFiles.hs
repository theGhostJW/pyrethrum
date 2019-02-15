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

tempFile :: RelFile -> IO (Either P.IOError AbsFile)
tempFile = subPath tempDir

logFile :: RelFile -> IO (Either P.IOError AbsFile)
logFile = subPath logDir

_tempFile = tempFile [relfile|demoTemp.txt|]

newtype FileExt = FileExt {unFileExt :: String}

logExtension :: FileExt
logExtension = FileExt ".log"

logFileSuffix :: String -> FileExt -> String
logFileSuffix suffix fileExt = "_" <> suffix <> unFileExt fileExt

fullLogFileName :: String -> String -> FileExt -> String
fullLogFileName prefix suffix fileExt = prefix <> logFileSuffix suffix fileExt

logFileName :: ZonedTime -> String -> FileExt -> String
logFileName now suffix fileExt =
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

    nowStr :: String
    nowStr = toStr $ formatTime defaultTimeLocale (toCharList "%F_%H-%M-%S") now
  in
    fullLogFileName (base36 msLeftInYear 7 <> "_" <> nowStr) suffix fileExt

logFilePath :: ZonedTime -> String -> FileExt -> IO (Either P.IOError AbsFile)
logFilePath now suffix fileExt = do
                          rf <- parseRelFileSafe $ toCharList $ logFileName now suffix fileExt
                          eitherf rf
                            (pure . Left . P.userError . toCharList . show)
                            logFile

handleFromPath :: Either P.IOError AbsFile -> IO (Either P.IOError (AbsFile, S.Handle))
handleFromPath = either
                  (pure . Left)
                  (\pth -> ((pth,) <$>) <$> (Right <$> S.openFile (toFilePath pth) S.WriteMode))

logFileNameAndHandle :: String -> FileExt -> IO (Either P.IOError (AbsFile, S.Handle))
logFileNameAndHandle fileNameSuffix fileExt = do
                                now <- getZonedTime
                                fp <- logFilePath now fileNameSuffix fileExt
                                handleFromPath fp

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

