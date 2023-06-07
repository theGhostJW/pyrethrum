{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module EffPlay.SpeakIOInterpreterStatic where

import qualified Data.Text.IO as T
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, runEff, (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)

-- Effect

data Speak :: Effect
data Speaker = Speaker
  { hello :: Text -> IO ()
  , goodbye :: Text -> IO ()
  }
newtype instance StaticRep Speak = Speak Speaker

type instance DispatchOf Speak = Static WithSideEffects

hello :: (Speak :> es) => Text -> Eff es ()
hello name = do
  Speak s <- getStaticRep
  unsafeEff_ $ s.hello name

goodbye :: (Speak :> es) => Text -> Eff es ()
goodbye name = do
  Speak s <- getStaticRep
  unsafeEff_ $ s.goodbye name

-- Interpreter

runSpeaker :: (HasCallStack, IOE :> es) => Speaker -> Eff (Speak : es) a -> Eff es a
runSpeaker speaker = evalStaticRep (Speak speaker)

-- Dynamic Params

staticImp :: Speaker
staticImp =
  Speaker
    { hello = \name -> T.putStrLn $ "Hello (static) " <> name
    , goodbye = \name -> T.putStrLn $ "Goodbye (static) " <> name
    }

staticImpCasual :: Speaker
staticImpCasual =
  Speaker
    { hello = \name -> T.putStrLn $ "hi (static) " <> name
    , goodbye = \name -> T.putStrLn $ "buy (static) " <> name
    }

-- Effectful Ap

speakApp :: (HasCallStack, IOE :> es) => Text -> Eff (Speak : es) ()
speakApp name = do
  hello name
  goodbye name

-- Implementation

-- $> exeSpeakerAppStatic 
exeSpeakerAppStatic :: IO ()
exeSpeakerAppStatic = do
  runEff . runSpeaker staticImp $ speakApp "John"

-- $> exeSpeakerAppStaticCasual
exeSpeakerAppStaticCasual :: IO ()
exeSpeakerAppStaticCasual = do
  runEff . runSpeaker staticImpCasual $ speakApp "John"