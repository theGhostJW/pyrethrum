module LogTransformation.Common where

import Common as C (AppError(..))
import Check as CK
import Pyrelude as P
import Pyrelude.IO
import Data.DList as D
import qualified Prelude as PO
import AuxFiles
import OrphanedInstances
import DSL.LogProtocol as LP
import Text.Show.Pretty as PP
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import System.IO as S
import Data.Functor
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Identity

newtype LineNo = LineNo { unLineNo :: Int } deriving (Show, Eq)

data LogTransformError =  LogDeserialisationError DeserialisationError |

                          LogIOError {
                                  message :: Text,
                                  error :: IOError
                                } |

                          LogTransformError {
                                  linNo :: LineNo,
                                  logItem :: LogProtocol,
                                  info :: Text
                                } deriving (Eq, Show)


data DeserialisationError  = DeserialisationError {
  linNo :: LineNo,
  errorTxt :: Text,
  line :: Either UnicodeException Text -- the type for decode UTF8
}  deriving (Eq, Show)

$(deriveJSON defaultOptions ''LogTransformError)
$(deriveJSON defaultOptions ''LineNo)
$(deriveJSON defaultOptions ''DeserialisationError)