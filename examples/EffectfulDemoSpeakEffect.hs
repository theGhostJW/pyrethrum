module EffectfulDemoSpeakEffect where

import Effectful as EF (
  Dispatch (Dynamic),
  DispatchOf,
  Effect,
 )
import Effectful.TH (makeEffect)

type instance DispatchOf Speak = Dynamic

data Speak :: Effect where
  Hello :: Text -> Speak m ()
  Goodbye :: Text -> Speak m ()
 
makeEffect ''Speak
