
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module InteractorSpike where

import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           System.Exit as SysExit                hiding (ExitCode (ExitSuccess))
import Foundation.Extended hiding (putStrLn)
import qualified Foundation.Extended as IOOps
import qualified Prelude

-- Example from: http://hackage.haskell.org/package/freer-simple-1.1.0.0/docs/Control-Monad-Freer.html#t:Eff

-- data FileSystem r where
--   ReadFile :: FilePath -> FileSystem String
--   WriteFile :: FilePath -> String -> FileSystem ()
--
-- runInMemoryFileSystem :: [(FilePath, String)] -> Eff (FileSystem ': effs) ~> Eff effs
-- runInMemoryFileSystem initVfs = let
--                                   fsToState :: Eff (FileSystem ': effs) ~> Eff (State [(FilePath, String)] ': effs)
--                                   fsToState = reinterpret $ case
--                                     ReadFile path -> do
--                                       vfs <- get
--                                       case lookup path vfs of
--                                         Just contents -> pure contents
--                                         Nothing -> error ("readFile: no such file " ++ path)
--
--                                     WriteFile path contents -> modify $ \vfs ->
--                                       (path, contents) : delete (path, contents) vfs
--                                  in
--                                    evalState initVfs . fsToState


-- Example: Console DSL: from Readme -  https://github.com/lexi-lambda/freer-simple

--------------------------------------------------------------------------------
                               -- Effect Model --
--------------------------------------------------------------------------------

data Console r where

  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console ()

printString :: Member Console effs => String -> Eff effs ()
printString = send . PutStrLn

getLine :: Member Console effs => Eff effs String
getLine = send GetLine

exitSuccess :: Member Console effs => Eff effs ()
exitSuccess = send ExitSuccess


--------------------------------------------------------------------------------
                          -- Effectful Interpreter --
--------------------------------------------------------------------------------

runConsole :: Eff '[Console, IO] a -> IO a
runConsole = runM . interpretM (
                                  \case
                                    PutStrLn msg -> IOOps.putStrLn msg
                                    GetLine -> toStr <$> Prelude.getLine
                                    ExitSuccess ->  SysExit.exitSuccess
                               )

--------------------------------------------------------------------------------
                            -- Pure Interpreter --
--------------------------------------------------------------------------------

data PureResult a = PureResult {
  result :: Either () a,
  interactionLog :: [String],
  remainingInputs :: [String]
} deriving Show

runConsolePure :: [String] -> Eff '[Console] w -> PureResult w
runConsolePure inputs instructions =  let
                                       invoke :: Console v -> Eff '[Error (), State [String], Writer [String]] v
                                       invoke (PutStrLn msg) = tell [">>> " <> msg]
                                       invoke GetLine = do
                                                     nextLine <- get
                                                     case nextLine of
                                                       [] -> error "not enough input"
                                                       (x:xs) -> tell ["<<< " <> x ] >> put xs >> pure x
                                       invoke ExitSuccess = throwError ()

                                       -- result :: ((Either () w, [String]), [String])
                                       (ansState, log) = run (runWriter (runState inputs (runError (reinterpret3 invoke instructions))))
                                     in
                                       PureResult {
                                         result = fst ansState,
                                         remainingInputs = snd ansState,
                                         interactionLog = log
                                       }

app :: (Member Console r) => Eff r ()
app = do
        printString "What is your name?: "
        name <- getLine
        printString $ "Nanu Nanu " <> name <> " have a nice day !!"
        printString "Bye !!"

demoEffectful :: IO ()
demoEffectful = runConsole app

demoPure :: PureResult ()
demoPure = runConsolePure ["Mork", "I need to go write a monad tutorial, nice to meet you"] app

demoPureFail:: PureResult ()
demoPureFail = runConsolePure [] app
