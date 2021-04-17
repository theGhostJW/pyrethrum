import Demo (salutation)
import Prelude

main :: IO ()
main = putStr salutation

-- >>> "Hello " <> salutation
-- "Hello Hello"
