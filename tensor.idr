module Tensor

-- import Data.HVect
import Data.Vect

-- ListType : (singleton : Bool) -> Type
-- ListType False = List Nat
-- ListType True = Nat

-- data Shape = NilShape
--            | Axis Nat (Shape)

-- ShapeType : (shape : List Nat) -> Shape
-- ShapeType [] = NilShape
-- ShapeType (x :: xs) = Axis x (ShapeType xs)

-- data Tensor : Type -> Shape -> Type
-- data Tensor a = Scalar a
--               | Vector {l: Nat} (Vect l a)

-- Interfaces.Eq (Shape s) where
--   ShapeNil == ShapeNil = True
--   ShapeNil == y = False
--   y == ShapeNil = False
--   (ShapeApp m x) == (ShapeApp m y) = True

data UnsafeShape = MkUnsafeShape (List Nat)
data SNat = MkSNat Nat

Eq UnsafeShape where
  (MkUnsafeShape xs) == (MkUnsafeShape ys) = xs == ys

data Shape : (shape : Vect n Nat) -> Type where
  ShapeNil  : Shape []
  ShapeApp  : (m : Nat) -> Shape s -> Shape (m :: s)

toUnsafe : Shape s -> UnsafeShape
toUnsafe ShapeNil = MkUnsafeShape []
toUnsafe (ShapeApp m xs) = let MkUnsafeShape xs' = toUnsafe xs in
                               MkUnsafeShape (m :: xs')

-- interface KnownNat (n : Nat) where
--   natSing : SNat
-- 
-- natVal : KnownNat n => Nat
-- natVal x = ?asdf
--
interface KnownNat (n : Nat) where
  fromNat : Nat -> Nat

interface MkSafeShape (s : Vect n Nat) where
   mkSafeShape : Shape s
 
MkSafeShape [] where
  mkSafeShape = ShapeNil

(MkSafeShape s, KnownNat m) => MkSafeShape (m :: s) where
  mkSafeShape = ?mkShapeShape

zipWith2 : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith2 f [] [] = []
zipWith2 f (x :: xs) (y :: ys) = f x y :: zipWith2 f xs ys


fromUnsafe : UnsafeShape -> Shape s
fromUnsafe (MkUnsafeShape []) = ?fromUnsafe_rhs_5
fromUnsafe (MkUnsafeShape (x :: xs)) = ?fromUnsafe_rhs_3

--
-- Ejemplos
-- 
-- fromUnsafe (MkUnsafeShape []) = ShapeNil
--fromUnsafe (MkUnsafeShape (x :: xs)) = ?fromUnsafe_2

--let shape' = toUnsafe  


-- fromUnsafe (MkUnsafeShape []) = ?nilshape
-- fromUnsafe (MkUnsafeShape (x :: xs)) = ShapeApp x (fromUnsafe (MkUnsafeShape xs))
