-- {-# LANGUAGE TypeData #-}
module CoreTypes where

-- type data When = Before | After | Around 
-- todo research type Data

data Before
data After
data Around

class When a

instance When Before
instance When After
instance When Around