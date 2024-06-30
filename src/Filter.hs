module Filter
  ( Filter (..),
    FilterResult (..),
    isAccepted,
    isRejected,
    applyFilters,
  )
where

data Filter rc t = MkFilter
  { rejectionDescription :: Text,
    predicate :: rc -> t -> Bool
  }

data FilterResult t = MkFilterResult
  { target :: t,
    rejection :: Maybe Text
  } deriving (Show, Eq, Functor)

isAccepted :: FilterResult t -> Bool
isAccepted = isNothing . (.rejection)

isRejected :: FilterResult t -> Bool
isRejected = isJust . (.rejection)

applyFilters :: forall rc t. [Filter rc t] -> rc -> t -> FilterResult t
applyFilters fltrs rc t = MkFilterResult t $ (.rejectionDescription) <$> find (\f -> not $ f.predicate rc t) fltrs