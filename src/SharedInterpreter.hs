{-# LANGUAGE QuasiQuotes #-}

module SharedInterpreter where

import           Control.Monad.Freer
import           Foundation.Extended hiding (writeFile)
import qualified  Prelude as P

data Nothing r where
 Nuttin :: Show s => s -> Nothing ()

nuttin :: (Show s, Member Nothing effs) => s -> Eff effs ()
nuttin = send . Nuttin

nuttinInterpreter :: Eff (Nothing ': effs) a -> Eff effs a
nuttinInterpreter = interpret $ \case
                          Nuttin s -> pure ()

data Logger r where
 Log :: Show s => s -> Logger ()

log :: (Show s, Member Logger effs) => s -> Eff effs ()
log = send . Log

logConsoleInterpreter :: LastMember IO effs => Eff (Logger ': effs) a -> Eff effs a
logConsoleInterpreter =  interpretM $ \case
                                         Log msg -> P.print msg

logNothingInterpreter :: Eff (Logger ': effs) a -> Eff effs a
logNothingInterpreter =  interpret $ \case
                                        Log msg -> pure ()

data FileSystem r where
  WriteFile :: Path a File -> String -> FileSystem ()

writeFile :: Member FileSystem effs => Path a File -> String -> Eff effs ()
writeFile pth = send . WriteFile pth

fileSystemIOInterpreter :: LastMember IO effs => Eff (FileSystem ': effs) a -> Eff effs a
fileSystemIOInterpreter = interpretM $ \case
                                        WriteFile path str -> writeFileUTF8 path str

fileSystemDoNothingInterpreter :: Eff (FileSystem ': effs) a -> Eff effs a
fileSystemDoNothingInterpreter = interpret $ \case
                                        WriteFile path str -> pure ()

interpretEveryThing :: Eff '[Nothing, FileSystem, Logger, IO] () -> IO ()
interpretEveryThing effs = runM $
                           logConsoleInterpreter $
                           fileSystemIOInterpreter $
                           nuttinInterpreter effs

interpretNothing :: Eff '[FileSystem, Logger, IO] () -> IO ()
interpretNothing effs = runM $
                           logNothingInterpreter $
                           fileSystemDoNothingInterpreter effs

justLog :: Members '[Logger] effs => Eff effs ()
justLog = log "Hello World"

logAndWrite :: Members '[FileSystem, Logger] effs => Eff effs ()
logAndWrite = do
                writeFile [absfile|C:\Vids\SystemDesign\Hello.txt|] "Hello World"
                log "Hello World"

demoJustLog :: IO ()
demoJustLog = interpretEveryThing justLog

demoLogAndWrite :: IO ()
demoLogAndWrite = interpretEveryThing logAndWrite

mergeThem :: [IO ()] -> IO ()
mergeThem = foldl' (>>) (pure ())

demoBoth :: IO ()
demoBoth = mergeThem [
                      interpretEveryThing justLog,
                      interpretEveryThing logAndWrite
                     ]

demoBothDry :: Members '[FileSystem, Logger, IO] effs => (Eff effs () -> IO ()) -> IO ()
demoBothDry intepreter = mergeThem $ intepreter <$> [
                      justLog,
                      logAndWrite
                    ]

demo = demoBothDry interpretEveryThing
demo1 = demoBothDry interpretNothing
