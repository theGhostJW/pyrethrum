
module DSL.LogProtocol where

import           DSL.Common
import           Foundation.Extended

data LogProtocol =
                   Message String |
                   Message' {
                               message :: String,
                               info    :: String
                             } |
                  Warning String |
                  Error AppError
                  deriving (Eq, Show)
