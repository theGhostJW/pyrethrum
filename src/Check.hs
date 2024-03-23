{-# LANGUAGE UndecidableInstances #-}

module Check (
  Check (..),
  TerminationStatus (..),
  Checks (..),
  CheckResult (..),
  CheckReport (..),
  chk,
  chk',
  assert,
  assert',
  applyCheck,
  skipChecks,
  skipCheck,
  mapRules,
  filterRules,
)
where

import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Data.Aeson.Types as AT (ToJSON (toJSON), Value (String))
import PyrethrumExtras (toS, (?))
import UnliftIO (MonadUnliftIO, tryAny)
import Prelude as P

-- import Prelude (Show (..), Read(..))
import GHC.Show (Show (..))
import GHC.Read (Read(..))
import BasePrelude (read)

-- import Hedgehog.Internal.Prelude (Show (..), Read(..))

data TerminationStatus = NonTerminal | Terminal deriving (Show, Read, Eq)

$(deriveToJSON defaultOptions ''TerminationStatus)

data Check ds = Check
  { terminationStatus :: TerminationStatus
  , message :: Maybe (ds -> Text)
  , header :: Text
  , rule :: ds -> Bool
  }

data CheckReadable = CheckLog
  { header :: Text
  , terminationStatus :: TerminationStatus
  }
  deriving (Show, Read)

chk :: Text -> (ds -> Bool) -> Checks ds
chk header rule = Checks [Check NonTerminal Nothing header rule]

chk' :: Text -> (ds -> Text) -> (ds -> Bool) -> Checks ds
chk' header message rule = Checks [Check NonTerminal (Just message) header rule]

assert :: Text -> (ds -> Bool) -> Checks ds
assert header rule = Checks [Check Terminal Nothing header rule]

assert' :: Text -> (ds -> Text) -> (ds -> Bool) -> Checks ds
assert' header message rule = Checks [Check Terminal (Just message) header rule]

instance Show (Check v) where
  show :: Check v -> String
  show Check{header, terminationStatus} = P.show $ CheckLog header terminationStatus

instance Read (Check v) where
  readsPrec :: Int -> String -> [(Check v, String)]
  readsPrec _ s = [(check, s)]
   where
    check =
      Check
        { terminationStatus = showable.terminationStatus
        , message = Nothing
        , header = showable.header
        , rule = const . error $ "Tried to call rule on a deserialised version of Check for: " <> toS showable.header
        }
    showable = read @CheckReadable s 

instance ToJSON (Check v) where
  toJSON :: Check v -> Value
  toJSON = String . toS . (.header)

newtype Checks ds = Checks
  { un :: [Check ds]
  }
  deriving (Show, Read, Semigroup, Monoid, IsList)

mapRules :: (Check ds -> Check ds') -> Checks ds -> Checks ds'
mapRules f = Checks . fmap f . coerce

filterRules :: (Check ds -> Bool) -> Checks ds -> Checks ds
filterRules f = Checks . P.filter f . coerce

data CheckResult
  = Pass
  | Skip
  | Fail
  deriving (Show, Eq, Ord)

data CheckReport
  = CheckReport
      { result :: CheckResult
      , message :: Text
      , info :: Text
      }
  | CheckApplicationFailed
      { header :: Text
      , exception :: Text
      , callStack :: Text
      }
  deriving (Show, Eq)

skipCheck :: Check ds -> CheckReport
skipCheck (Check{header}) = CheckReport Skip header "Validation skipped"

skipChecks :: Checks ds -> [CheckReport]
skipChecks chks = skipCheck <$> chks.un

-- need to do this in an error handling context so we can catch and report
-- exceptions thrown applying the check
applyCheck :: (MonadUnliftIO m) => ds -> TerminationStatus -> Check ds -> m (CheckReport, TerminationStatus)
applyCheck ds termStatus r =
  do
    rslt <-
      tryAny
        . pure
        $ case termStatus of
          Terminal -> (report Skip, Terminal)
          NonTerminal ->
            first report
              $ r.rule ds
                ? (Pass, NonTerminal)
              $ (Fail, r.terminationStatus)

    rslt
      & either
        ( \e ->
            pure
              ( CheckApplicationFailed
                  { header = r.header
                  , exception = toS $ displayException e
                  , callStack = toS $ prettyCallStack callStack
                  }
              , NonTerminal
              )
        )
        pure
 where
  report rslt = CheckReport rslt r.header (r.message & maybe "" (ds &))

$(deriveJSON defaultOptions ''CheckResult)
$(deriveJSON defaultOptions ''CheckReport)
$(deriveToJSON defaultOptions ''Checks)
