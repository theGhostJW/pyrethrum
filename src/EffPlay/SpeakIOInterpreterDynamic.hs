{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module EffPlay.SpeakIOInterpreterDynamic (
  runSpeak,
  exeSpeakerApp
) where

import EffPlay.SpeakEffect 
import BasePrelude (IOException)
import Control.Monad.Catch (catch, handle)
import qualified DSL.Internal.FileSystemRawIO as R
import Effectful as EF (
  Eff,
  IOE,
  liftIO,
  type (:>), runEff,
 )

import DSL.FileSystemEffect (FSException (..), FileSystem (..))
import Effectful.Dispatch.Dynamic (
  HasCallStack,
  LocalEnv,
  interpret,
  localSeqUnliftIO,
 )
import qualified Effectful.Error.Static as E

runSpeak :: forall es a. (HasCallStack, IOE :> es) => Eff (Speak : es) a -> Eff es a
runSpeak =
  interpret $ \_ -> EF.liftIO . \case 
    Hello name -> print $ "Hello (dynamic) " <> name
    Goodbuy name -> print  $ "Goodbuy (dynamic) " <> name


speakApp :: (HasCallStack, IOE :> es) => Text -> Eff (Speak : es) ()
speakApp name = do
  hello name
  goodbuy name

exeSpeakerApp :: IO ()
exeSpeakerApp = do
  runEff . runSpeak $ speakApp "John"