{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Check
  ( Check (..),
    FailStatus (..),
    Checks (..),
    CheckResult (..),
    CheckReport (..),
    chk,
    chk',
    assert,
    assert',
    applyCheck,
    filterRules,
    listChecks,
    mapRules,
    skipChecks,
    skipCheck,
    -- TODO - more checks chkFalse', ChkEmpty, chkNotEmpty, chkEqual, chkNotEqual, chkContains, tagged variants
    -- chkNotContains, chkMatches, chkNotMatches, chkLessThan, chkLessThanOrEqual, chkGreaterThan, chkGreaterThanOrEqual
  )
where

-- import Prelude (Show (..), Read(..))

import BasePrelude (read)
import Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import Data.Aeson.Types as AT (ToJSON (toJSON), Value (String))
import GHC.Read (Read (..))
import GHC.Show (Show (..))
import PyrethrumExtras (toS, (?))
import UnliftIO (MonadUnliftIO, tryAny)
import Prelude as P

-- import Hedgehog.Internal.Prelude (Show (..), Read(..))

data FailStatus = NonTerminal | Terminal deriving (Show, Read, Eq)

$(deriveToJSON defaultOptions ''FailStatus)

data Check vs = Check
  { -- failStatus:
    -- NonTerminal for regular checks (suceeding checks will be run)
    -- Terminal for asserts (suceeding checks will not be run)
    header :: Text,
    message :: Maybe (vs -> Text),
    failStatus :: FailStatus,
    rule :: vs -> Bool
  }

data CheckReadable = CheckLog
  { header :: Text,
    failStatus :: FailStatus
  }
  deriving (Show, Read)

singleton :: Text -> Maybe (vs -> Text) -> FailStatus -> (vs -> Bool) -> Checks vs
singleton hdr msg fs rule = Checks [Check hdr msg fs rule]

chk :: Text -> (vs -> Bool) -> Checks vs
chk header = singleton header Nothing NonTerminal

chk' :: Text -> (vs -> Text) -> (vs -> Bool) -> Checks vs
chk' header message = singleton header (Just message) NonTerminal

assert :: Text -> (vs -> Bool) -> Checks vs
assert header = singleton header Nothing Terminal

-- todo: play with labelling values to see if useful similar to falsify: https://well-typed.com/blog/2023/04/falsify/?utm_source=pocket_reader#predicates
assert' :: Text -> (vs -> Text) -> (vs -> Bool) -> Checks vs
assert' header message = singleton header (Just message) Terminal

instance Show (Check v) where
  show :: Check v -> String
  show Check {header, failStatus} = P.show $ CheckLog header failStatus

instance Read (Check v) where
  readsPrec :: Int -> String -> [(Check v, String)]
  readsPrec _ s = [(check, s)]
    where
      check =
        Check
          { failStatus = showable.failStatus,
            message = Nothing,
            header = showable.header,
            rule = const . error $ "Tried to call rule on a deserialised version of Check for: " <> toS showable.header
          }
      showable = read @CheckReadable s

instance ToJSON (Check v) where
  toJSON :: Check v -> Value
  toJSON = String . toS . (.header)

newtype Checks vs = Checks
  { un :: [Check vs]
  }
  deriving (Show, Read)
  deriving newtype (Semigroup, Monoid, IsList)

mapRules :: (Check vs -> Check vs') -> Checks vs -> Checks vs'
mapRules f = Checks . fmap f . coerce

filterRules :: (Check vs -> Bool) -> Checks vs -> Checks vs
filterRules f = Checks . P.filter f . coerce

data CheckResult
  = Pass
  | Skip
  | Fail
  deriving (Show, Eq, Ord, Generic, NFData)

data CheckReport
  = CheckReport
      { header :: Text,
        message :: Text,
        result :: CheckResult
      }
  | CheckApplicationFailed
      { header :: Text,
        exception :: Text,
        callStack :: Text
      }
  | CheckListing
      { header :: Text
      }
  deriving (Show, Eq, Generic, NFData)

skipCheck :: Check vs -> CheckReport
skipCheck (Check {header}) = CheckReport header "Validation skipped" Skip

skipChecks :: Checks vs -> [CheckReport]
skipChecks chks = skipCheck <$> chks.un

listChecks :: Checks vs -> [CheckReport]
listChecks chks = listCheck <$> chks.un
 where 
  listCheck :: Check vs -> CheckReport
  listCheck (Check {header}) = CheckListing header

-- need to do this in an error handling context so we can catch and report
-- exceptions thrown applying the check
applyCheck :: (MonadUnliftIO m) => vs -> FailStatus -> Check vs -> m (CheckReport, FailStatus)
applyCheck vs failStatus ck =
  do
    rslt <-
      tryAny
        . pure
        $ case failStatus of
          Terminal -> (report Skip, Terminal)
          NonTerminal ->
            first report
              $ ck.rule vs
                ? (Pass, NonTerminal)
              $ (Fail, ck.failStatus)

    rslt
      & either
        ( \e ->
            pure
              ( CheckApplicationFailed
                  { header = ck.header,
                    exception = toS $ displayException e,
                    callStack = toS $ prettyCallStack callStack
                  },
                NonTerminal
              )
        )
        pure
  where
    report = CheckReport ck.header (ck.message & maybe "" (vs &))

$(deriveJSON defaultOptions ''CheckResult)
$(deriveJSON defaultOptions ''CheckReport)
$(deriveToJSON defaultOptions ''Checks)
