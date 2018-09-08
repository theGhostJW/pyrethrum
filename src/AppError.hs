
module AppError where

import           Foundation.Extended

data AppError = NotImplemented String |
                GenericError String |
                EnsureError String |
                ReadFileError IOException |
                WriteFileError IOException |
                IOError IOException
                deriving (Show, Eq)
