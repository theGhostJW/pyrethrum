{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Console where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import System.Exit hiding (ExitCode(ExitSuccess))
import Foundation.Extended
import qualified Prelude

--------------------------------------------------------------------------------
                               -- Effect Model --
--------------------------------------------------------------------------------
data Console r where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console ()

putStrLn' :: Member Console effs => String -> Eff effs ()
putStrLn' = send . PutStrLn

getLine' :: Member Console effs => Eff effs String
getLine' = send GetLine

exitSuccess' :: Member Console effs => Eff effs ()
exitSuccess' = send ExitSuccess

--------------------------------------------------------------------------------
                          -- Effectful Interpreter --
--------------------------------------------------------------------------------
runConsole :: Eff '[Console, IO] a -> IO a
runConsole = runM . interpretM (\case
  PutStrLn msg -> putStrLn msg
  GetLine -> toStr <$> Prelude.getLine
  ExitSuccess -> exitSuccess)

--------------------------------------------------------------------------------
                             -- Pure Interpreter --
--------------------------------------------------------------------------------
data PureResult a = PureResult {
  result :: Either () a,
  interactionLog :: [String],
  remainingInputs :: [String]
} deriving Show

runConsolePure :: [String] -> Eff '[Console] w -> PureResult w
runConsolePure inputs req =
  PureResult {
    result = fst rs,
    remainingInputs = snd rs,
    interactionLog = log
  }
  where
    (rs, log) = run (runWriter (runState inputs (runError (reinterpret3 go req))))

    go :: Console v -> Eff '[Error (), State [String], Writer [String]] v
    go (PutStrLn msg) = tell [">>> " <> msg]
    go GetLine = get >>= \case
      [] -> error "insufficient input"
      (x:xs) -> tell ["<<< " <> x ] >> put xs >> pure x
    go ExitSuccess = throwError ()

--------------------------------------------------------------------------------
                             -- The Application --
--------------------------------------------------------------------------------
app :: (Member Console r) => Eff r ()
app = do
        putStrLn' "What is your name?: "
        name <- getLine'
        putStrLn' $ "Nanu Nanu " <> name <> " have a nice day !!"
        putStrLn' "Bye !!"

--------------------------------------------------------------------------------
                             -- Demos --
--------------------------------------------------------------------------------
demoEffectful :: IO ()
demoEffectful = runConsole app

demoPure :: PureResult ()
demoPure = runConsolePure
            ["Mork",
             "I need to go write a monad tutorial, nice to meet you"]
            app

demoPureFail:: PureResult ()
demoPureFail = runConsolePure [] app
