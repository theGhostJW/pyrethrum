module Internal.SuiteValidation (
  chkSuite,
  SuiteValidationError (..),
) where

import Data.Set qualified as S
import Filter
import PyrethrumExtras ((?))
import Prelude hiding (All, atomically, id, newEmptyTMVarIO, newTVarIO, readMVar)

data SuiteValidationError = Failure
  { failure :: Text,
    notes :: Text
  }
  deriving (Show, Eq)

chkSuite :: [FilterResult Text] -> Maybe SuiteValidationError
chkSuite r = emptySuite r <|> duplicateTitle r

emptySuite :: [FilterResult Text] -> Maybe SuiteValidationError
emptySuite r =
  any accepted r ? Nothing $
    Just $
      Failure
        { failure = "Filtered Test Suite is Empty",
          notes =
            "The test suite is empty after filtering:\n"
              <> "Check the filter log and change the relevant test or run config to ensure some fixtures are run."
        }

duplicateTitle :: [FilterResult Text] -> Maybe SuiteValidationError
duplicateTitle r =
  firstDuplicateFixtureTitle r
    <&> \dupe ->
      Failure
        { failure = "Duplicate Fixture Title",
          notes =
            "The following fixture title is duplicated in the test suite:\n"
              <> dupe
              <> "\n"
              <> "Fixture titles must be unique, please change the title of one of these fixtures."
        }

firstDuplicateFixtureTitle :: [FilterResult Text] -> Maybe Text
firstDuplicateFixtureTitle = firstDuplicate . fmap (.target)
  where
    -- todo make polymorphic and add to pyrelude (replace existing)
    firstDuplicate :: [Text] -> Maybe Text
    firstDuplicate = go S.empty
      where
        go _ [] = Nothing
        go seen (x : xs)
          | x `S.member` seen = Just x
          | otherwise = go (S.insert x seen) xs
