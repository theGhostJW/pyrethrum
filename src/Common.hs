
module Common where

import           Control.Monad.Freer.Writer
import           Foundation.Extended as F
import           Foundation.List.DList
import Data.Aeson.TH
import OrphanedInstances
import           Foundation.String as S
import Text.Show.Pretty as PP
import qualified Prelude as P
import Basement.String as S

showPretty :: Show a => a -> String
showPretty = toS . ppShow

indentString :: Int -> String -> String
indentString i s = 
  let 
    linesClean :: [String]
    linesClean = fst . F.breakEnd (not . S.all (' ' ==)) $ S.lines s

    unlined :: P.String
    unlined = P.unlines $ (\s' -> s == "" ? "" $ toS $ F.replicate (CountOf i) ' ' <> s')  <$> linesClean
  in 
    toS $ safeLast unlined == Just '\n' ? P.init unlined $ unlined  


data PreTestStage = Rollover |
                    GoHome
                    deriving (Show, Eq)

$(deriveJSON defaultOptions ''PreTestStage)

data DetailedInfo = DetailedInfo {
            message :: String,
            info    :: String
          }
          deriving (Eq, Show)

$(deriveJSON defaultOptions ''DetailedInfo)

newtype EnsureError = EnsureError String deriving (Show, Eq)

$(deriveJSON defaultOptions ''Common.EnsureError)

data FilterError = InvalidItemFilter String |
                   DuplicateItemId Int String deriving (Eq, Show)

$(deriveJSON defaultOptions ''FilterError)

data FileSystemError =
    ReadFileError IOException |
    WriteFileError IOException
    deriving (Show, Eq)

$(deriveJSON defaultOptions ''FileSystemError)

data AppError =
            AppFileSystemError FileSystemError |
            AppEnsureError EnsureError |
            AppFilterError FilterError |

            AppNotImplementedError String |

            AppGenericError String |

            AppUserError String |
            AppUserError' DetailedInfo |

            AppPreTestError PreTestStage String AppError |
            AppPreTestCheckExecutionError PreTestStage String AppError|
            AppPreTestCheckError PreTestStage String |

            AppIOError IOException |
            AppIOError' String IOException

            deriving (Show, Eq)

$(deriveJSON defaultOptions ''AppError)

type WriterDList = Writer (DList String)

dList :: Show s => s -> DList String
dList s = fromList [show s]
