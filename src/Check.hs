
module Check where

import           Foundation.Extended

data CheckItemResult = Success String |
                       Failure String |
                       Skipped String |
                       Exception String
                       deriving (Show, Eq)
