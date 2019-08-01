
module Common where

import           Pyrelude as P
import  qualified        Data.DList as D
import Data.Aeson.TH
import OrphanedInstances
import Text.Show.Pretty as PP
import Polysemy.Output as O

indentText :: Int -> Text -> Text
indentText i s = 
  let 
    linesClean :: [Text]
    linesClean = fst . P.breakEnd (not . all (' ' ==)) $ lines s

    unlined :: Text
    unlined = unlines $ (\s' -> s == "" ? "" $ toS $ replicateText i " " <> s')  <$> linesClean
  in 
    toS $ 
          last unlined /= Just '\n' 
            ? unlined   
            $ maybef (init unlined)
                ""
                id 

data PreTestStage = Rollover |
                    GoHome
                    deriving (Show, Eq)

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

type WriterDListP = O.Output (D.DList Text)

dList :: Show s => s -> D.DList Text
dList s = D.fromList [txt s]

$(deriveJSON defaultOptions ''PreTestStage)