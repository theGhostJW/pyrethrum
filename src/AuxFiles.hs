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
auxBase = subDirFromBaseDir binDir [reldir|.auxFiles|]

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

logFileName :: ZonedTime -> String -> String
logFileName now suffix =
  let
    msLeftInYear :: Integer
    msLeftInYear =  let
                    utcNow = zonedTimeToUTC now
                    (y, m, d)  = toGregorian $ utctDay $ utcNow
                    nyd = UTCTime (fromGregorian (y + 1) 1 1) 0
                    daysDif = diffDays (utctDay nyd) (utctDay utcNow)
                    msPerDay = 24 * 60 * 60 * 1000
                    timeDifms = P.round $ fromIntegral (diffTimeToPicoseconds $ utctDayTime nyd P.- utctDayTime utcNow) / 1000000000
                  in
                    daysDif * msPerDay + timeDifms

    nowStr :: String
    nowStr = toStr $ formatTime defaultTimeLocale (toCharList "%F %H_%M_%S") now
  in
    base36 msLeftInYear 7 <> " " <> nowStr <> " " <> suffix <> ".log"

logFilePath :: ZonedTime -> String -> IO (Either P.IOError AbsFile)
logFilePath now suffix = do
                          rf <- parseRelFileSafe $ toCharList $ logFileName now suffix
                          eitherf rf
                            (pure . Left . P.userError . toCharList . show)
                            logFile

logFileHandle :: IO (Either P.IOError (AbsFile, S.Handle))
logFileHandle = do
                  now <- debug' "Zoned" <$> getZonedTime
                  fp <- logFilePath now "raw"
                  eitherf fp
                    (pure . Left)
                    (\pth -> ((pth,) <$>) <$> (Right <$> S.openFile (toFilePath pth) S.WriteMode))

-- based on https://gist.github.com/jdeseno/9501557
base36 :: Integer -> Int -> String
base36 num minWidth =
  let 
    conv :: Int -> [Char] -> [Char]
    conv n s = (C.chr $ n + 48 + ((n-9) `div` (-27) * (-7))) : s

    units :: Integer -> [Int]
    units n 
      | n < (36 :: Integer) = [fromIntegral n] 
      | otherwise = (units (n `div` (36 :: Integer))) <> [fromIntegral n `P.rem` 36]

    unpadded :: [Char]
    unpadded = foldr conv [] $ units num

    len :: Int
    len = fromCount $ length unpadded

    prefix :: [Char]
    prefix = (fromIntegral len) < minWidth ? replicate (toCount (minWidth - len)) '0' $ []
  in
    toStr $ prefix <> (foldr conv [] $ units num)

