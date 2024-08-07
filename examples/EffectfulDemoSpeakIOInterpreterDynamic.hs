module EffectfulDemoSpeakIOInterpreterDynamic where

import Data.Text.IO qualified as T
import Effectful as EF (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  liftIO,
  runEff,
  type (:>),
 )
import Effectful.Dispatch.Dynamic (
  interpret
 )
import Effectful.TH (makeEffect)

-- Effect

type instance DispatchOf Speak = Dynamic

data Speak :: Effect where
  Hello :: Text -> Speak m ()
  Goodbye :: Text -> Speak m ()

makeEffect ''Speak

-- Interpreters

runSpeak :: forall es a. ( IOE :> es) =>Eff (Speak : es) a -> Eff es a
runSpeak =
  interpret $ \_ ->
    EF.liftIO . \case
      Hello name -> T.putStrLn $ "Hello " <> name
      Goodbye name -> T.putStrLn $ "Goodbye " <> name

runSpeakCasual :: forall es a. ( IOE :> es) =>Eff (Speak : es) a -> Eff es a
runSpeakCasual =
  interpret $ \_ ->
    EF.liftIO . \case
      Hello name -> T.putStrLn $ "hi " <> name
      Goodbye name -> T.putStrLn $ "bye " <> name

-- Effectful Ap

speakApp :: (Speak :> es) => Text -> Eff es ()
speakApp name = do
  hello name
  goodbye name

-- Implementation

-- $ > exeSpeakerApp
exeSpeakerApp :: IO ()
exeSpeakerApp =
  runEff . runSpeak $ speakApp "John"

-- $ > exeSpeakerAppCasual
exeSpeakerAppCasual :: IO ()
exeSpeakerAppCasual =
  runEff . runSpeakCasual $ speakApp "John"
