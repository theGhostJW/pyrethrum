module List.Extra ( 
  module Data.List.Extra,
  List.Extra.last,
  List.Extra.init,
  List.Extra.maximum,
  List.Extra.head, 
  count
) where 

--  shims for relude to ultimately be included in a revived pyrelude

import Data.List.Extra as L hiding (unlines, lines)
import Prelude hiding (last)
import Data.List.Extra hiding (last, init, maximum, head) 
import qualified Data.List.Extra as DE
import PyrethrumExtras ((?))


-- need to hide in p(relude) too - lines / unlines

safel :: ([a] -> b) -> [a] -> Maybe b
safel unsafef l = 
    l & \case 
      [] -> Nothing 
      _ -> Just $ unsafef l

last:: [a] -> Maybe a
last = safel L.last 

head :: [a] -> Maybe a
head = safel L.head 

init :: [a] -> Maybe [a]
init = safel L.init  

maximum :: Ord a => [a] -> Maybe a
maximum = safel L.maximum

count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\n x -> p x ? succ n $ n) 0

-- pyrelude ii
{-
- add Extras
- UnitTest
- head tail => use maybe
- asum and friends (parser combinators)
- ppShow abnd friends -. Test
- add tests
- unlift IO  
- path / path IO
- investigate / document - ‘show’ is not a (visible) method of class ‘Show’typeche
- copy / modify Hlint
-}