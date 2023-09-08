module DSL.Internal.ApEvent where

import qualified Data.Aeson as A
import LogTransformation.Common (IterationPhase (Parse))
import Data.Aeson.TH (deriveJSON, defaultOptions)

-- TODO: make log effect requiring out


-- TODO: make log effect requiring out
data ULog
  = StartFolder Text
  | EndFolder Text
  | Log Text
  | Log'
      { message :: Text
      , details :: Text
      }
  | Warning Text
  | Warning'
      { message :: Text
      , details :: Text
      }
  | Error Text
  | Error'
      { message :: Text
      , details :: Text
      }
  deriving stock (Eq, Show)

newtype ApStateJSON = ApStateJSON {unApStateJSON :: A.Value} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''ApStateJSON)

newtype DStateJSON = DStateJSON {unDStateJSON :: A.Value} deriving (Eq, Show, IsString)
$(deriveJSON defaultOptions ''DStateJSON)

data FLog
  = Action
  | Parse ApStateJSON
  | Check DStateJSON
  | Step Text
  | Step'
      { message :: Text
      , details :: Text
      }
  deriving stock (Eq, Show)

data ApEvent
  = User ULog
  | Framework FLog
  deriving stock (Eq, Show)
