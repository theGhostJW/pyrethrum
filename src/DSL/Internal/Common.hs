
module DSL.Internal.Common where

import           Control.Monad.Freer.Writer
import           Foundation.Extended
import           Foundation.List.DList

type WriterDList = Writer (DList String)

dList :: Show s => s -> DList String
dList s = fromList [show s]
