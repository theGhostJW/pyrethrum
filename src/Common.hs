
module Common where

import           Control.Monad.Freer.Writer
import           Pyrelude as F
import           Pyrelude.Data.Text.Hidden as T
import           Data.DList
import Data.Aeson.TH
import OrphanedInstances
import Text.Show.Pretty as PP
import qualified Prelude as P

showPretty :: Show a => a -> Text
showPretty = toS . ppShow

indentText :: Int -> Text -> Text
indentText i s = 
  let 
    linesClean :: [Text]
    linesClean = fst . F.breakEnd (not . T.all (' ' ==)) $ lines s

    unlined :: Text
    unlined = unlines $ (\s' -> s == "" ? "" $ toS $ T.replicate i " " <> s')  <$> linesClean
  in 
    toS $ 
          T.last unlined /= Just '\n' 
            ? unlined   
            $ maybef (T.init unlined)
                ""
                id 

data PreTestStage = Rollover |
                    GoHome
                    deriving (Show, Eq)

$(deriveJSON defaultOptions ''PreTestStage)

data DetailedInfo = DetailedInfo {
            message :: Text,
            info    :: Text
          }
          deriving (Eq, Show)

$(deriveJSON defaultOptions ''DetailedInfo)

newtype EnsureError = EnsureError Text deriving (Show, Eq)

$(deriveJSON defaultOptions ''Common.EnsureError)

data FilterError = InvalidItemFilter Text |
                   DuplicateItemId Int Text deriving (Eq, Show)

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

            AppNotImplementedError Text |

            AppGenericError Text |
            AppGenericError' Text Text |

            AppUserError Text |
            AppUserError' DetailedInfo |

            AppPreTestError PreTestStage Text AppError |
            AppPreTestCheckExecutionError PreTestStage Text AppError|
            AppPreTestCheckError PreTestStage Text |

            AppIOError IOException |
            AppIOError' Text IOException

            deriving (Show, Eq)

$(deriveJSON defaultOptions ''AppError)

type WriterDList = Writer (DList Text)

dList :: Show s => s -> DList Text
dList s = fromList [txt s]
