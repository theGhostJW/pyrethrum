
module AppError where

import           Foundation.Extended

data AppError = NotImplemented String |
                GenericError String |
                IOError IOException
                deriving (Show, Eq)
