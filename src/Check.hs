{-# LANGUAGE UndecidableInstances #-}

module Check (
  Check (..),
  FailStatus (..),
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

data FailStatus = NonTerminal | Terminal deriving (Show, Read, Eq)

$(deriveToJSON defaultOptions ''FailStatus)

data Check ds = Check
  { -- terminationStatus: 
    -- NonTerminal for regular checks (suceeding checks will be run)
    -- Terminal for asserts (suceeding checks will not be run)
    failStatus :: FailStatus
  , message :: Maybe (ds -> Text)
  , header :: Text
  , rule :: ds -> Bool
  }

data CheckReadable = CheckLog
  { header :: Text
  , failStatus :: FailStatus
  }
  deriving (Show, Read)

chk :: Text -> (ds -> Bool) -> Checks ds
chk header rule = Checks [Check NonTerminal Nothing header rule]

chk' :: Text -> (ds -> Text) -> (ds -> Bool) -> Checks ds
chk' header message rule = Checks [Check NonTerminal (Just message) header rule]

assert :: Text -> (ds -> Bool) -> Checks ds
assert header rule = Checks [Check Terminal Nothing header rule]

-- todo: play with labelling values to see if useful similar to falsify: https://well-typed.com/blog/2023/04/falsify/?utm_source=pocket_reader#predicates
assert' :: Text -> (ds -> Text) -> (ds -> Bool) -> Checks ds
assert' header message rule = Checks [Check Terminal (Just message) header rule]


instance Show (Check v) where
  show :: Check v -> String
  show Check{header, failStatus} = P.show $ CheckLog header failStatus

instance Read (Check v) where
  readsPrec :: Int -> String -> [(Check v, String)]
  readsPrec _ s = [(check, s)]
   where
    check =
      Check
        { failStatus = showable.failStatus
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
applyCheck :: (MonadUnliftIO m) => ds -> FailStatus -> Check ds -> m (CheckReport, FailStatus)
applyCheck ds failStatus ck =
  do
    rslt <-
      tryAny
        . pure
        $ case failStatus of
          Terminal -> (report Skip, Terminal)
          NonTerminal ->
            first report
              $ ck.rule ds
                ? (Pass, NonTerminal)
              $ (Fail, ck.failStatus)

    rslt
      & either
        ( \e ->
            pure
              ( CheckApplicationFailed
                  { header = ck.header
                  , exception = toS $ displayException e
                  , callStack = toS $ prettyCallStack callStack
                  }
              , NonTerminal
              )
        )
        pure
 where
  report rslt = CheckReport rslt ck.header (ck.message & maybe "" (ds &))

$(deriveJSON defaultOptions ''CheckResult)
$(deriveJSON defaultOptions ''CheckReport)
$(deriveToJSON defaultOptions ''Checks)
