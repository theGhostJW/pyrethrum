-- logspot.com/2012/04/less-travelled-monad-tutorial-part-1.html?m=1
{-# LANGUAGE UndecidableInstances #-}

module DataKinds where

import           Data.Maybe
import           Foundation.Extended
import qualified Prelude

-----------------------------------------------
{- http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html -}
-----------------------------------------------


newtype Orange = Orange Integer
data Apple = Apple

data Bag a where
  OrangeBag :: [Orange] -> Bag [Orange]
  AppleBag :: [Apple] -> Bag [Apple]

oranges2 = OrangeBag [Orange 0, Orange 1]
noOranges = OrangeBag []

getOrange :: Bag [Orange] -> Orange
getOrange (OrangeBag x) = case x of
                            [] -> Orange 0
                            h: t -> h


-----------------------------------------------
{- http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html -}
-----------------------------------------------

data NNat = Nought | Nxt NNat

data Nought
data Nxt a

type One = Nxt Nought
type Two = Nxt One
type Three = Nxt Two
type Four = Nxt (Nxt (Nxt (Nxt Nought)))

{-
  But this is pretty unsatisfying. After all, there’s nothing that stops us from saying Succ Bool,
  which doesn’t make any sense. I’m pretty sold on the benefits of types for clarifying thinking
  and preventing errors, so abandoning the safety of types when I program my types just seems silly.

  In order to get that safety back, we need to introduce more kinds than merely *. For this, we have
  to level up our GHC.

  Data Kinds

  {-# LANGUAGE DataKinds #-}

-}

-- data Nat = Zero | Succ Nat
{-
λ> :kind 'Zero
'Zero :: Nat
λ> :kind 'Succ
'Succ :: Nat -> Nat

λ> :type Zero
Zero :: Nat
λ> :type Succ
Succ :: Nat -> Nat
-}

--- {-# LANGUAGE GADTs #-} ---

data MyMaybe a where
    Just :: a -> MyMaybe a
    Nothing :: MyMaybe a

{-
The GADT syntax lists the constructors line-by-line, and instead of providing the fields of the constructor,
we provide the type signature of the constructor. This is an interesting change – I just wrote out a -> Maybe a.

That suggests, to me, that I can make these whatever type I want.
-}

data IntBool a where
   Int :: Int -> IntBool Int
   Bool :: Bool -> IntBool Bool

extractIntBool :: IntBool a -> a
extractIntBool (Int i)  = i
extractIntBool (Bool b) = b

extractIntBoolInt = extractIntBool $ Int 5
extractIntBoolB = extractIntBool $ Bool False

-- Vectors --

data Nat = Zero | Succ Nat deriving Show

data Vector (n :: Nat) a where
  VNil :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where
  show VNil         = "VNil"
  show (VCons a as) = toList $ "VCons " <> show a <> " (" <> show as <> ")"

-- add value level example
add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ n) m = add n (Succ m)

addZero = add Zero $ Succ (Succ Zero)
-- Succ (Succ Zero)

addTwo = add (Succ (Succ Zero)) $ Succ (Succ Zero)

type family Add n m where
  Add 'Zero n = n
  Add ('Succ n) m = 'Succ (Add n m)

-- Where we used :kind to inspect the kind of types, we can use :kind! to evaluate
-- these types as far as GHC can.
-- This snippet illustrates the difference:

{-
  :kind  Add ('Succ ('Succ 'Zero)) ('Succ 'Zero)
    >> Add ('Succ ('Succ 'Zero)) ('Succ 'Zero) :: Nat

  :kind! Add ('Succ ('Succ 'Zero)) ('Succ 'Zero)
  Add ('Succ ('Succ 'Zero)) ('Succ 'Zero) :: Nat
   = 'Succ ('Succ ('Succ 'Zero))

-- Bad Implementations
append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil rest = VNil

• Could not deduce: m ~ 'Zero
    from the context: n ~ 'Zero
      bound by a pattern with constructor:
                 VNil :: forall a. Vector 'Zero a,
               in an equation for ‘append’
      at /home/matt/Projects/dep-types.hs:31:8-11
    ‘m’ is a rigid type variable bound by
      the type signature for:
        append :: forall (n :: Nat) a (m :: Nat).
                  Vector n a -> Vector m a -> Vector (Add n m) a
      at /home/matt/Projects/dep-types.hs:30:11
    Expected type: Vector (Add n m) a
      Actual type: Vector 'Zero a
  • In the expression: VNil
    In an equation for ‘append’: append VNil rest = VNil
  • Relevant bindings include
      rest :: Vector m a
        (bound at /home/matt/Projects/dep-types.hs:31:13)
      append :: Vector n a -> Vector m a -> Vector (Add n m) a
        (bound at /home/matt/Projects/dep-types.hs:31:1)

-}

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil xs = xs
append (VCons a rest) xs = VCons a (append rest xs)

appendDemo = append (VCons 1 (VCons 3 VNil)) (VCons 2 VNil)

-- append (VCons a rest) xs = append rest (VCons a xs)
-- append (VCons 1 (VCons 3 VNil)) (VCons 2 VNil)
-- VCons 3 (VCons 1 (VCons 2 (VNil))) X - wrong

-- append (VCons a rest) xs = VCons a (append rest xs) ~ Does not compile
{-
src\DataKinds.hs:142:28-51: error:
    * Could not deduce: Add n1 ('Succ m) ~ 'Succ (Add n1 m)
      from the context: n ~ 'Succ n1
        bound by a pattern with constructor:
                   VCons :: forall a (n :: Nat).
                            a -> Vector n a -> Vector ('Succ n) a,
                 in an equation for `append'
        at src\DataKinds.hs:142:9-20
      Expected type: Vector (Add n m) a
        Actual type: Vector ('Succ (Add n1 m)) a
    * In the expression: VCons a (append rest xs)
      In an equation for `append':
          append (VCons a rest) xs = VCons a (append rest xs)
    * Relevant bindings include
        xs :: Vector m a (bound at src\DataKinds.hs:142:23)
        rest :: Vector n1 a (bound at src\DataKinds.hs:142:17)
        append :: Vector n a -> Vector m a -> Vector (Add n m) a
          (bound at src\DataKinds.hs:141:1)
    |
142 | append (VCons a rest) xs = VCons a (append rest xs)
    |                            ^^^^^^^^^^^^^^^^^^^^^^^^

--  We can kinda see what went wrong if we lay the Vector, Add and append definitions next to each other:

data Vector n a where
    VNil :: Vector Zero a
    VCons :: a -> Vector n a -> Vector (Succ n) a

type family Add x y where
    Add 'Zero n = n
    Add ('Succ n) m = Add n ('Succ m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil xs           = xs
append (VCons a rest) xs = VCons a (append rest xs)

error: append (VCons a rest) xs = VCons a (append rest xs)
 break down∷
   append (VCons a rest) xs =

   Here, we know that VCons a rest has the type Vector (Succ n) a, and xs has the type Vector m a.
   We need to produce a result of type Vector (Add (Succ n) m) a in order for the type to line up right.
   We use VCons a (append rest xs). VCons has a length value that is the Successor of the result of
   append rest xs, which should have the value Add n m, so the length there is Succ (Add n m).

   Unfortunately, our result type needs to be Add (Succ n) m.

 We know these values are equivalent. Unfortuantely, GHC cannot prove this, so it throws up it’s hands.
 Two definitions, which are provably equivalent, are structurally different, and this causes the
 types and proofs to fail.

 This is a HUGE gotcha in type level programming – the implementation details matter,
 a lot, and they leak, hard. We can fix this by using a slightly different definition of Add:

    Add ('Succ n) m = 'Succ (Add n m)
-}

-- HList

data HList xs where
    HNil :: HList '[]
    (:::) :: a -> HList as -> HList (a ': as)

infixr 6 :::

hlst = 'a' ::: 1 ::: "hello" ::: HNil

-- instance Show (HList xs) where
--     show HNil         = "HNil"
--     show (x ::: rest) = toList $ "_ ::: " <> show rest

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show (HList as), Show a) => Show (HList (a ': as)) where
    show (a ::: rest) =
        toList $ show a <> " ::: " <> show rest



-----------------------------------------------
  -- http://softwaresimply.blogspot.com/2012/04/less-travelled-monad-tutorial-part-1.html?m=1
-----------------------------------------------
newtype Distance = Dist Double
newtype Mass = Mass Double

data Pair a = MkPair a a


data Tuple a b = Tuple a b
newtype HardAA a = HardAA (a Int)
-- (* -> *) -> *

{-
data FileSystem r where
  ReadFile :: FilePath -> FileSystem String
  WriteFile :: FilePath -> String -> FileSystem ()

-}

data HardA a where
   HardA :: Int -> a Int -> HardA (a Int)

instance Prelude.Show a  => Prelude.Show (HardA a)  where
   show harda = "Hard A of Wtf"

-- data C a b = C (a b) Int
-- data C a b where
--   C :: Show a => Int -> (b -> a) -> C (a b) Int

-- c = C (HardA 3) 3

data HardB a b c = HardB (a b) (c a Int)
{-
  a -> b -> c -> *
  a: * -> *
  b: *
  (* -> *) -> b -> c -> *
  (* -> *) -> * -> (a -> Int -> *) -> *
  (* -> *) -> * -> ((* -> *) -> * -> *) -> *


-- (* -> *) -> * -> ((* -> *) -> * -> *) -> *

-}

-- test = HardB (HardA "Hello")

-- Fuck this Shit
