{-# LANGUAGE QuasiQuotes #-}

module AuxFiles where

import Foundation.Extended as F
import qualified Prelude as P
import Paths_pyrethrum
import Data.Time
import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import qualified System.IO as S

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

timeStamp :: IO ()
timeStamp = do
              today <- utctDay <$> getCurrentTime
              undefined


logFileName :: UTCTime -> String -> String
logFileName now suffix =
  let
    leftInYear =  let
                    (y, m, d)  = toGregorian $ utctDay now
                    nyd = UTCTime (fromGregorian (y + 1) 1 1) 0
                    daysDif = diffDays (utctDay nyd) (utctDay now)
                    msPerDay = 24 * 60 * 60 * 1000
                    timeDifm = fromIntegral (diffTimeToPicoseconds $ utctDayTime nyd P.- utctDayTime now) / 1000000000000
                  in
                    fromIntegral (daysDif * msPerDay) + timeDifm

    nowStr :: String
    nowStr = toStr $ formatTime defaultTimeLocale (toCharList "%F %H_%M_%S") now
  in
    show leftInYear <> " " <> nowStr <> " " <> suffix <> ".log"

_logFileName = do
                putStrLn $ logFileName (UTCTime (fromGregorian 2019 1 10) 86399) "raw"
                putStrLn $ logFileName (UTCTime (fromGregorian 2019 1 10) 86399.547) "raw"
                putStrLn $ logFileName (UTCTime (fromGregorian 2019 1 10) 700) "raw"
                putStrLn $ logFileName (UTCTime (fromGregorian 2019 1 20) 500) "raw"


logFilePath :: UTCTime -> String -> IO (Either P.IOError AbsFile)
logFilePath now suffix = do
                          rf <- parseRelFileSafe $ toCharList $ logFileName now suffix
                          eitherf rf
                            (pure . Left . P.userError . toCharList . show)
                            logFile

logFileHandle :: IO (Either P.IOError (AbsFile, S.Handle))
logFileHandle = do
                  now <- getCurrentTime
                  fp <- logFilePath now "raw"
                  eitherf fp
                    (pure . Left)
                    (\pth -> ((pth,) <$>) <$> (Right <$> S.openFile (toFilePath pth) S.WriteMode))

--_logFileHandle =  do
