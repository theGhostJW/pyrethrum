
module Common where

import           Prelude as P
import  qualified        Data.DList as D
import Data.Aeson.TH
import Polysemy.Output as O
import Data.Text as T
import Control.Exception (IOException)


indentText :: Int -> Text -> Text
indentText i s =  undefined

data PreTestStage = Rollover |
                    GoHome
                    deriving (Show, Eq)

data DetailedInfo = DetailedInfo {
            message :: Text,
            info    :: Text
          }
          deriving (Eq, Show)

$(deriveJSON defaultOptions ''DetailedInfo)

data FilterErrorType = InvalidItemFilter Text |
                        DuplicateItemId Int Text deriving (Eq, Show)

$(deriveJSON defaultOptions ''FilterErrorType)

data FileSystemErrorType = ReadFileError | WriteFileError
    deriving (Show, Eq)

$(deriveJSON defaultOptions ''FileSystemErrorType)

data FrameworkError e =
            Error Text |
            Error' DetailedInfo |

            FileSystemError FileSystemErrorType P.IOError |
            EnsureError Text |
            FilterError FilterErrorType |

            NotImplementedError Text |

            PreTestError PreTestStage Text (FrameworkError e) |
            PreTestCheckExecutionError PreTestStage Text (FrameworkError e)|
            PreTestCheckError PreTestStage Text |

            IOError IOException |
            IOError' Text IOException |

            AnnotatedError Text (FrameworkError e) |

            SuiteError e
            deriving (Show, Eq, Functor)

            

type OutputDListText = O.Output (D.DList Text)

dList :: Show s => s -> D.DList Text
dList s = undefined

$(deriveJSON defaultOptions ''PreTestStage)