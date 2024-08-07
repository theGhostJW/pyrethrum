{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

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
  -- HasCallStack,
  interpret,
 )
import Effectful.TH (makeEffect)

-- Effect

type instance DispatchOf Speak = Dynamic

data Speak :: Effect where
  Hello :: Text -> Speak m ()
  Goodbye :: Text -> Speak m ()

makeEffect ''Speak

-- Interpreters

runSpeak :: forall es a. (HasCallStack, IOE :> es) => Eff (Speak : es) a -> Eff es a
runSpeak =
  interpret $ \_ ->
    EF.liftIO . \case
      Hello name -> T.putStrLn $ "Hello (dynamic) " <> name
      Goodbye name -> T.putStrLn $ "Goodbye (dynamic) " <> name

runSpeakCasual :: forall es a. (HasCallStack, IOE :> es) => Eff (Speak : es) a -> Eff es a
runSpeakCasual =
  interpret $ \_ ->
    EF.liftIO . \case
      Hello name -> T.putStrLn $ "hi (dynamic) " <> name
      Goodbye name -> T.putStrLn $ "bye (dynamic) " <> name

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