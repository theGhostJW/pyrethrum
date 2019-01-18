
{-# OPTIONS_GHC -fno-warn-orphans #-}

module OrphanedInstances where

import           Data.Yaml
import           Foundation.Extended


instance ToJSON String where 
  toJSON cl = _ 