
module DSL.Internal.Common where

import           Control.Monad.Freer.Writer
import           Foundation.Extended
import           Foundation.List.DList

type WriterDList = Writer (DList String)

data PreTestStage = Rollover |
                    GoHome
                    deriving (Show, Eq)

newtype EnsureError = EnsureError String deriving (Show, Eq)

data FileSystemError =
    ReadFileError IOException |
    WriteFileError IOException
    deriving (Show, Eq)

data AppError =
            AppFileSystemError FileSystemError |
            AppEnsureError EnsureError |

            NotImplementedError String |
            GenericError String |

            PreTestError PreTestStage String AppError |
            PreTestCheckExecutionError PreTestStage String AppError |
            PreTestCheckError PreTestStage String |

            IOError IOException

            deriving (Show, Eq)

dList :: Show s => s -> DList String
dList s = fromList [show s]
