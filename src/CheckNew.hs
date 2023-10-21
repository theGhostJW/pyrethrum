{-# LANGUAGE UndecidableInstances #-}

module CheckNew (
  Check (..),
  TerminationStatus (..),
  Checks,
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
import Data.Function (($), (&), (.))
import qualified Data.List as L
import Data.Text (Text, foldr')
import GHC.Show (Show (..))
import LogTransformation.PrintLogDisplayElement (IterationWarning (warning))
import PyrethrumExtras (toS, uu, (?))
import UnliftIO (MonadUnliftIO, tryAny)
import Prelude as P

data TerminationStatus = NonTerminal | Terminal deriving (Show, Eq)

$(deriveToJSON defaultOptions ''TerminationStatus)

data Check ds = Check
  { terminationStatus :: TerminationStatus
  , message :: Maybe (ds -> Text)
  , header :: Text
  , rule :: ds -> Bool
  }

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
  show ck = toS ck.header

instance ToJSON (Check v) where
  toJSON :: Check v -> Value
  toJSON = String . toS . (.header)

newtype Checks ds = Checks
  { un :: [Check ds]
  }
  deriving (Show, Semigroup, Monoid, IsList)

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
