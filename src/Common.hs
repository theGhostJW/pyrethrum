
module Common where

import Pyrelude as P
    ( fst,
      ($),
      Eq(..),
      Functor,
      Show,
      Semigroup((<>)),
      Int,
      Maybe(Just),
      Text,
      IOError,
      IOException,
      Category(id, (.)),
      (<$>),
      not,
      breakEnd,
      replicateText,
      maybef,
      txt,
      toS,
      (?),
      lines,
      unlines,
      Listy(init, all, last), maybe, fromMaybe, debug'_ )
import  qualified        Data.DList as D
import Data.Aeson.TH ( defaultOptions, deriveJSON )
import OrphanedInstances()
import Polysemy.Output as O ( Output )

data HookLocation = BeforeAll | 
                    AfterAll | 
                    BeforeEach |
                    AfterEach deriving (Eq, Show)

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
            $ fromMaybe "" (init unlined)

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

            -- TODO Change this for hooks
            PreTestError Text (FrameworkError e) |
            PreTestCheckExecutionError Text (FrameworkError e)|
            PreTestCheckError Text |

            IOError IOException |
            IOError' Text IOException |

            AnnotatedError Text (FrameworkError e) |

            SuiteError e
            deriving (Show, Eq, Functor)

$(deriveJSON defaultOptions ''FrameworkError)

type OutputDListText = O.Output (D.DList Text)

dList :: Show s => s -> D.DList Text
dList s = D.fromList [txt s]

$(deriveJSON defaultOptions ''HookLocation)