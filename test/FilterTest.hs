module FilterTest where

import Filter
import PyrethrumExtras.Test

filters :: Filters Int Int
filters =
  Filtered
    [ MkFilter "Odd" \rc n -> odd $ n + rc,
      MkFilter "NotD3" (\rc n -> 0 /= mod (n + rc) 3)
    ]

r :: t -> Maybe Text -> FilterResult t
r = MkFilterResult

-- >>> unit_applyFilters_on_empty
unit_applyFilters_on_empty :: IO ()
unit_applyFilters_on_empty =
  [] ... applyFilters filters 0 <$> []

-- >>> unit_applyFilters
unit_applyFilters :: IO ()
unit_applyFilters =
  (uncurry r <$> zip [1, 2, 3, 4, 5, 6] [Nothing, Just "Odd", Just "NotD3", Just "Odd", Nothing, Just "Odd"])
    ... (applyFilters filters 0 <$> [1, 2, 3, 4, 5, 6])

-- >>> unit_unfiltered
unit_unfiltered :: IO ()
unit_unfiltered =
  (uncurry r . (, Nothing) <$> [1, 2, 3, 4, 5, 6])
    ... (applyFilters Unfiltered 0 <$> [1, 2, 3, 4, 5, 6])
