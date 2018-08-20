
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
import           Data.List
import           System.Exit as SysExit                hiding (ExitCode (ExitSuccess))
import Foundation.Extended as F hiding (putStrLn)
import qualified Foundation.Extended as IOOps
import Foundation.String
import qualified Prelude

--  Example from: https://github.com/lexi-lambda/freer-simple/blob/master/examples/src/Capitalize.hs

data Capitalize v where
  Capitalize :: String -> Capitalize String

capitalize :: Member Capitalize r => String -> Eff r String
capitalize = send . Capitalize

capRunner :: Capitalize x -> Eff r x
capRunner (Capitalize s) = pure $ upper s

-- like reinterpret but where the interpreter needs no effects ??
-- interpret :: forall eff effs. (eff ~> Eff effs) -> Eff (eff ': effs) ~> Eff effs

runCapitalize :: Eff (Capitalize ': r) w -> Eff r w
runCapitalize = interpret capRunner

runCapitalizeIO :: Eff '[Capitalize, IO] a -> IO a
runCapitalizeIO = runM . interpretM capIOInterpretor

capIOInterpretor :: Capitalize ~> IO
capIOInterpretor (Capitalize s) = let
                                    u = upper s
                                  in
                                    do
                                       IOOps.putStrLn u
                                       pure u


capApp :: (Member Capitalize effs) => Eff effs String
capApp = capitalize "freer monads woo hoo"

capDemo :: String
capDemo = run $ runCapitalize capApp

capDemoIO :: IO String
capDemoIO = runCapitalizeIO capApp


-- Example from: http://hackage.haskell.org/package/freer-simple-1.1.0.0/docs/Control-Monad-Freer.html#t:Eff

type FilePath = String
data FileSystem r where
  ReadFile :: FilePath -> FileSystem String
  WriteFile :: FilePath -> String -> FileSystem ()

deriving instance Show a => Show (FileSystem a)

-- type level bind like function that prepends effects to the stack
--
--                                              lifter:
--                                                commandParam -> efectfulEffect
--                                                  command runner
--                                                  effect algebra -> Effs              pure scripted eff           effectful eff
-- reinterpret  :: forall f g effs.                (a ~> Eff (b ': effs))               -> Eff (a ': effs)      ~> Eff ( b': effs)
-- reinterpret2 :: forall f g h effs.              (f ~> Eff (g ': (h ': effs)))        -> Eff (f ': effs)      ~> Eff (g ': (h ': effs))
-- reinterpret3 :: forall f g h i effs.            (f ~> Eff (g ': (h ': (i ': effs)))) -> Eff (f ': effs)      ~> Eff (g ': (h ': (i ': effs)))
-- reinterpretN :: forall gs f effs. Weakens gs => (f ~> Eff (gs :++: effs))            -> Eff (f ': effs)      ~> Eff (gs :++: effs)
   -- breaks type inference

runInMemoryFileSystem :: [(FilePath, String)] -> Eff (FileSystem : effs) x -> Eff effs (x, [(FilePath, String)])
runInMemoryFileSystem initVfs = let
                                  --
                                  -- reinterpret :: forall f g effs. (a ~> Eff (b ': effs)) -> Eff (a ': effs) ~> Eff ( b': effs)
                                  fsToState :: Eff (FileSystem ': effs) ~> Eff (State [(FilePath, String)] ': effs)
                                  fsToState = reinterpret runCommand

                                  -- command runner
                                  runCommand :: FileSystem ~> Eff (State [(FilePath, String)] ': effs)
                                  runCommand = \case
                                                  ReadFile path -> do
                                                                    vfs <- get
                                                                    case Prelude.lookup path vfs of
                                                                      Just contents -> pure contents
                                                                      Nothing -> error ("readFile: no such file " <> path)

                                                  WriteFile path contents -> modify $
                                                    \vfs -> (path, contents) : deleteBy (\t0 t1 -> fst t0 == fst t1) (path, contents) vfs

                                  initFileSystem :: Eff (State [(FilePath, String)] ': effs) x -> Eff effs (x, [(FilePath, String)])
                                  initFileSystem = runState initVfs
                                 in
                                   initFileSystem . fsToState


-- lift algebra into the effect context
-- send ~ “Sends” an effect, which should be a value defined as part of an effect algebra
-- (see the module documentation for Control.Monad.Freer), to an effectful computation.
-- This is used to connect the definition of an effect to the Eff monad so that it can
-- be used and handled.
readFile :: Member FileSystem effs => FilePath -> Eff effs String
readFile = send . ReadFile

writeFile :: Member FileSystem effs => FilePath -> String -> Eff effs ()
writeFile path = send . WriteFile path


fileApp :: (Member FileSystem fs) => Eff fs String
fileApp = do
            writeFile "temp.txt" "Nii"
            writeFile "temp.txt" "Hello"
            content <- readFile "temp.txt"
            pure $ "Content: " <> content


demo = run $ runInMemoryFileSystem [] fileApp



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

consoleEffectRunner :: Console a -> IO a
consoleEffectRunner = \case
                          PutStrLn msg -> IOOps.putStrLn msg
                          GetLine -> toStr <$> Prelude.getLine
                          ExitSuccess ->  SysExit.exitSuccess

runConsole :: Eff '[Console, IO] a -> IO a
runConsole = runM . interpretM consoleEffectRunner

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
