module CoreUtils where

import UnliftIO.Concurrent qualified as C
import BasePrelude (read)
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)

type ThreadId = Int

-- ThreadId 5 -> 5
mkThreadId :: C.ThreadId -> ThreadId
mkThreadId = read . drop 9 . show

newtype PException = MkException {displayText :: [Text]} deriving (Show, Eq, Ord)

exceptionTxt :: SomeException -> PException
exceptionTxt e = TE.PException $ txt <$> lines (displayException e)

$(deriveToJSON defaultOptions ''PException)
