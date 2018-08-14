
{-# LANGUAGE NoTemplateHaskell #-}

module NaturalTransformations where

import           Foundation

---- Functor Examples ----

-- Const - import Control.Applicative (Const(Const)) --

newtype Const a b = Const { getConst :: a } deriving Show

instance Functor (Const a) where
  fmap _ (Const a) = Const a

object1 = Const "Hello"
object2 = (&& False) <$> object1 -- Functor f => f Bool
object3 = (\_ -> 1.2 :: Double) <$> object2 -- Functor f => f Double

final = getConst object3 -- "hello"

-- ID - import Data.Functor.Identity --

newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

demo = runIdentity $ (5 *) <$> Identity 5

-- Function: base (->) --
data Laggy a b = Laggy a b deriving Show

instance Functor (Laggy a) where
  -- fmap = (.)
  fmap :: (b -> c) -> Laggy a b -> Laggy a c
  fmap f (Laggy a b)  = Laggy a (f b)

addFive :: Int -> Laggy Int Int
addFive i = Laggy i (i + 5)

demoLaggy = (* 10) <$> addFive 5

(##>) = Laggy

demoLaggyInfix = (* 10) <$> 5 ##> 10

{-
  A Natural Transofmation Transforms the Context where as functord transform
  the content

  Many / ? most polymorphic functions are natural transformations especially as
  there is Const and Idenity

  so a -> m a
  == Const a -> m a
   and identity 
-}
