
module DSL.Internal.Common where

import qualified Control.Monad              as Monad
import           Control.Monad.Freer.Writer
import           Foundation.Extended
import           Foundation.List.DList
import qualified Prelude                    as P


type WriterDList = Writer (DList String)

dList :: Show s => s -> DList String
dList s = fromList [show s]
