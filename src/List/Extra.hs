{-# OPTIONS_GHC -fno-warn-x-partial#-} 

module List.Extra (
  module Data.List.Extra,
  List.Extra.last,
  List.Extra.init,
  List.Extra.maximum,
  List.Extra.minimum,
  List.Extra.head,
  List.Extra.tail,
  count,
  zipWithIndex,
) where

--  shims for relude to ultimately be included in a revived pyrelude

import Data.List.Extra hiding (head, tail, init, last, maximum, minimum)
import Data.List.Extra as L hiding (lines, unlines)
import PyrethrumExtras ((?))
import Prelude hiding (last)

-- need to hide in p(relude) too - lines / unlines

safel :: ([a] -> b) -> [a] -> Maybe b
safel unsafef l =
  l & \case
    [] -> Nothing
    _ -> Just $ unsafef l

last :: [a] -> Maybe a
last = safel L.last

head :: [a] -> Maybe a
head = safel L.head

tail :: [a] -> Maybe [a]
tail = safel L.tail

init :: [a] -> Maybe [a]
init = safel L.init

maximum :: (Ord a) => [a] -> Maybe a
maximum = safel L.maximum

minimum :: (Ord a) => [a] -> Maybe a
minimum = safel L.minimum

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count p = foldl' (\n x -> p x ? succ n $ n) 0

zipWithIndex :: (Num a, Enum a, Foldable t) => t b -> [(a, b)]
zipWithIndex = zip [0 ..] . toList