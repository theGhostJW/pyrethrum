
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

-- Example: Console DSL:  https://github.com/lexi-lambda/freer-simple

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
                                    GetLine -> toString <$> Prelude.getLine
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

demoInstructions :: (Member Console r) => Eff r ()
demoInstructions = do
                      printString "What is your name?: "
                      name <- getLine
                      printString $ "Hello " <> name <> " have a nice day !!"

demoPure :: PureResult ()
demoPure = runConsolePure ["John", "I need to go write a monad tutorial nice to meet you"] demoInstructions

demoPureFail:: PureResult ()
demoPureFail = runConsolePure [] demoInstructions

demoEffectful :: IO ()
demoEffectful = runConsole demoInstructions
