{-# LANGUAGE DeriveAnyClass #-}
module Filter
  ( Filter (..),
    FilterResult (..),
    Filters (..),
    accepted,
    rejected,
    applyFilters,
  )
where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveToJSON, deriveToJSON)

data Filters rc fc = Filtered [Filter rc fc] | Unfiltered

data Filter rc t = MkFilter
  { description :: Text,
    predicate :: rc -> t -> Bool
  }

data FilterResult t = MkFilterResult
  { target :: t,
    rejection :: Maybe Text
  }
  deriving (Show, Eq, Functor, Generic, NFData)

accepted :: FilterResult t -> Bool
accepted = isNothing . (.rejection)

rejected :: FilterResult t -> Bool
rejected = isJust . (.rejection)

applyFilters :: forall rc t. Filters rc t -> rc -> t -> FilterResult t
applyFilters fltrs rc t =
  case fltrs of
    Unfiltered -> MkFilterResult t Nothing
    Filtered fltrs' -> MkFilterResult t $ (.description) <$> find (\f -> not $ f.predicate rc t) fltrs'

$(deriveToJSON defaultOptions ''FilterResult)