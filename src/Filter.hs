module Filter
  ( Filter (..),
    FilterResult (..),
    Filters (..),
    isAccepted,
    isRejected,
    applyFilters,
  )
where

data Filters rc fc = Filtered [Filter rc fc] | Unfiltered

data Filter rc t = MkFilter
  { description :: Text,
    predicate :: rc -> t -> Bool
  }

data FilterResult t = MkFilterResult
  { target :: t,
    rejection :: Maybe Text
  }
  deriving (Show, Eq, Functor)

isAccepted :: FilterResult t -> Bool
isAccepted = isNothing . (.rejection)

isRejected :: FilterResult t -> Bool
isRejected = isJust . (.rejection)

applyFilters :: forall rc t. Filters rc t -> rc -> t -> FilterResult t
applyFilters fltrs rc t =
  case fltrs of
    Unfiltered -> MkFilterResult t Nothing
    Filtered fltrs' -> MkFilterResult t $ (.description) <$> find (\f -> not $ f.predicate rc t) fltrs'