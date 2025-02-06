module Utils where

import Data.Aeson
  ( Key,
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (..),
    object, fromJSON, Result (..),
  )

-- Utility functions will be added here

opt :: (Functor f, KeyValue e b, ToJSON a) => Key -> f a -> f b
opt lbl mb = (lbl .=) <$> mb
