module FilterTest where

import Filter
import PyrethrumExtras.Test

fltrs :: [Filter Int Int]
fltrs =
  [ MkFilter "Odd" \rc n -> odd $ n + rc,
    MkFilter "NotD3" (\rc n -> 0 /= mod (n + rc) 3)
  ]


r :: t -> Maybe Text -> FilterResult t
r = MkFilterResult

-- >>> unit_empty_applyFilters
unit_empty_applyFilters :: IO ()
unit_empty_applyFilters =
  [] ... applyFilters fltrs 0 <$> []


-- >>> unit_applyFilters
unit_applyFilters :: IO ()
unit_applyFilters =
 (uncurry r <$> zip [1, 2, 3, 4, 5, 6] [Nothing, Just "Odd", Just "NotD3", Just "Odd", Nothing, Just "Odd"]) ...  applyFilters fltrs 0 <$> [1, 2, 3, 4, 5, 6]
