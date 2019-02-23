
module Common where

import           Control.Monad.Freer.Writer
import           Foundation.Extended
import           Foundation.List.DList
import Data.Aeson.TH
import OrphanedInstances

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
