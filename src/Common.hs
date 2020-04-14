
module Common where

import           Pyrelude as P
import  qualified        Data.DList as D
import Data.Aeson.TH
import OrphanedInstances()
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

data FilterError = InvalidItemFilter Text |
                   DuplicateItemId Int Text deriving (Eq, Show)

$(deriveJSON defaultOptions ''FilterError)

data FileSystemErrorType = ReadFileError | WriteFileError
    deriving (Show, Eq)

$(deriveJSON defaultOptions ''FileSystemErrorType)

data AppError =
            FileSystemError FileSystemErrorType IOError |
            EnsureError Text |
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
            AppIOError' Text IOException |

            AppAnnotatedError Text AppError
            deriving (Show, Eq)

$(deriveJSON defaultOptions ''AppError)

type OutputDListText = O.Output (D.DList Text)

dList :: Show s => s -> D.DList Text
dList s = D.fromList [txt s]

$(deriveJSON defaultOptions ''PreTestStage)