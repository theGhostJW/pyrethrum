
module AppError where

import           Foundation.Extended

data AppError = InvalidItemFilter String |
                NotImplemented String |
                GenericError String |
                EnsureError String |
                ReadFileError IOException |
                WriteFileError IOException |
                IOError IOException
                deriving (Show, Eq)
