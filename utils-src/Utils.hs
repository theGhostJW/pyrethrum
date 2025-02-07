module Utils where

import Data.Aeson
  ( Key,
    KeyValue ((.=)),
    ToJSON ()
  )

import Data.Text (Text, pack)
-- Utility functions will be added here

opt :: (Functor f, KeyValue e b, ToJSON a) => Key -> f a -> f b
opt lbl mb = (lbl .=) <$> mb

txt :: Show a => a -> Text
txt = pack . show