module ReaderTry where

import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Data.Maybe
import Foundation.Extended
import           Data.Map           as Map

type Bindings = Map String Int

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = run $ runReader bindings calcIsCountCorrect

-- The Reader effect, which implements this complicated check.
calcIsCountCorrect :: Eff '[Reader Bindings] Bool
calcIsCountCorrect = do
                      count <- asks (lookupVar "count")
                      bindings <- ask :: Eff '[Reader Bindings] Bindings
                      pure $ count == Map.size bindings

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Bindings -> Int
lookupVar name bindings = fromJust (Map.lookup name bindings)

sampleBindings :: Map.Map String Int
sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]

main :: IO ()
main = putStrLn
  $ "Count is correct for bindings " <> show sampleBindings <> ": "
  <> show (isCountCorrect sampleBindings)
