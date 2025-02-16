module E2ETest where

import Test.Tasty.HUnit as HUnit

-- todo: test extras - split off

(===) :: (Eq a, Show a, HasCallStack)
  => a -- ^ The actual value
  -> a -- ^ The expected value
  -> Assertion
(===) = (@=?)


-- >>> unit_temp_demo
unit_temp_demo :: IO ()
unit_temp_demo = 1 === 2


