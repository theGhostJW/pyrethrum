
{-# OPTIONS_GHC -fno-warn-orphans #-}

module OrphanedInstances where

import           Data.Yaml
import           Data.Aeson.Types
import           Pyrelude
import           GHC.IO.Exception
import Data.Aeson.TH
import qualified Prelude as P

$(deriveJSON defaultOptions ''IOErrorType)

instance ToJSON IOException where 
  toJSON IOError {..} =  object [
    "ioe_type" .= ioe_type,
    "ioe_location"  .= ioe_location,  
    "ioe_description"  .= ioe_description,  
    "ioe_errno"  .= show ioe_errno,  
    "ioe_filename"  .= ioe_filename  
    ]

instance FromJSON IOException where 
  parseJSON = withObject "ioException" $ \o -> do
    ioe_type <- o .: "ioe_type"
    ioe_location <- o .: "ioe_location"
    ioe_description <- o .: "ioe_description"
    ioe_errno <- P.read <$> o .: "ioe_errno"
    ioe_filename <- o .: "ioe_filename"
    pure IOError{ioe_handle = Nothing, ..}