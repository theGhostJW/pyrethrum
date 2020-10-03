{-# LANGUAGE QuasiQuotes #-}

module AuxFiles where

import Pyrelude as F
import Pyrelude.IO
import qualified Prelude as P
import System.Environment
import Paths_pyrethrum
import qualified System.IO as S
import qualified Data.Char as C
import Control.Monad.IO.Class

data WantConsole = Console | NoConsole deriving Eq

exeDir :: IO AbsDir
exeDir = parent <$> (parseAbsFile =<< getExecutablePath)

auxBase :: IO (Either P.IOError AbsDir)
auxBase = subDirFromBaseDir exeDir [reldir|auxFiles|]

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
dataFile = subPath dataDir

_tempFile = tempFile [relfile|demoTemp.txt|]

newtype FileExt = FileExt {unFileExt :: Text} deriving (Show, Eq, Ord)

logFileExt :: FileExt
logFileExt = FileExt ".log"

-- based on https://gist.github.com/jdeseno/9501557
base36 :: Integer -> Int -> Text
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
    len = length unpadded

    prefix :: P.String
    prefix = fromIntegral len < minWidth ? replicate (minWidth - len) '0' $ []
  in
    toS $ prefix <> foldr conv [] (units num)

logFilePrefix :: Time -> Text
logFilePrefix currentTime =
      let
        nextYear =  (getYear . dateYear . datetimeDate . timeToDatetime $ currentTime) + 1
        
        nyd = timeFromYmdhms nextYear 1 1 0 0 0
        msLeftInYear :: Integer
        msLeftInYear = fromIntegral (getTimespan . width $ nyd .... currentTime) `div` 1000000
      in 
        base36 msLeftInYear 7 <> "_" <> toS (encode_YmdHMS SubsecondPrecisionAuto (DatetimeFormat (Just '_') (Just '_') (Just '-')) (timeToDatetime  currentTime))
 

logFilePath :: Maybe Text -> Text -> FileExt -> IO (Either P.IOError (Text, AbsFile))
logFilePath mNamePrefix suffix fileExt = 
  do
    pfx <- maybef mNamePrefix
              (logFilePrefix <$> now)
              pure
    relPath <- parseRelFileSafe $ pfx <> "_" <> suffix <> unFileExt fileExt
    eitherf relPath
      (pure . Left . P.userError . toS . show)
      (\relFle -> ((pfx,) <$>) <$> logFile relFle)

-- toDo  - move to extended
safeOpenFile :: AbsFile -> S.IOMode -> IO (Either P.IOError S.Handle)
safeOpenFile pth mode = 
  catchIOError (Right <$> S.openFile (toFilePath pth) mode) (pure . Left)

data HandleInfo = HandleInfo {
  prefix :: Text,
  path ::  AbsFile,
  fileHandle :: S.Handle
}

logFileHandle :: Text -> FileExt -> IO (Either P.IOError HandleInfo)
logFileHandle fileNameSuffix fileExt = do
                                        ethPth <- logFilePath Nothing fileNameSuffix fileExt
                                        eitherf ethPth 
                                          (pure . Left)
                                          (\(pfx, pth) -> (HandleInfo pfx pth <$>) <$> safeOpenFile pth S.WriteMode)


-- Writing temp files used mostly used for de bugging 
defaultTempFileName :: RelFile
defaultTempFileName = [relfile|temp.txt|]

listToText :: Show a => [a] -> Text
listToText lst = unlines $ txt <$> lst

toTempBase' :: WantConsole -> Text -> RelFile -> IO ()
toTempBase' wantConsole txt' fileName = 
  do 
    ethQPth <- tempFile fileName
    eitherf ethQPth
      (\e -> putStrLn $ "failed to generate path: " <> txt e)
      (\adsPath ->
        do 
          let 
            destPath = toFilePath adsPath 
          when (wantConsole == Console) 
            (putStrLn $ "temp file written to: " <> toS destPath)
          writeFile destPath txt'
      )
 

toTemp' :: Text -> RelFile -> IO ()
toTemp' = toTempBase' Console

toTemp :: Text -> IO ()
toTemp = flip toTemp' defaultTempFileName

toTempFromList' :: Show a => [a] -> RelFile -> IO ()
toTempFromList' = toTemp' . listToText

toTempFromList :: Show a => [a] -> IO ()
toTempFromList = flip toTempFromList' defaultTempFileName

toTempNoConsole' :: Text -> RelFile -> IO ()
toTempNoConsole' = toTempBase' NoConsole

toTempNoConsole :: Text -> IO ()
toTempNoConsole = flip toTempNoConsole' defaultTempFileName

toTempFromListNoConsole' :: Show a => [a] -> RelFile -> IO ()
toTempFromListNoConsole' = toTempNoConsole' . listToText

toTempFromListNoConsole :: Show a => [a] -> IO ()
toTempFromListNoConsole = flip toTempFromList' defaultTempFileName
