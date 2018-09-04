
module AppError where

import           Foundation.Extended

data AppError = GenericError String |
                EnsureError String |
                ReadFileError IOException |
                WriteFileError IOException |
                IOError IOException
                deriving Show
