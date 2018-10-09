module Tensor

import Data.Vect

--
-- Unsafe shape
--
data UnsafeShape = MkUnsafeShape (List Nat)

Eq UnsafeShape where
  (MkUnsafeShape xs) == (MkUnsafeShape ys) = xs == ys

transposeUnsafe : UnsafeShape -> UnsafeShape
transposeUnsafe (MkUnsafeShape xs) = MkUnsafeShape (reverse xs) 

--
-- Safe shape
--
data Shape : (shape : Vect dim Nat) -> Type where
  ShapeNil  : Shape []
  ShapeApp  : (m : Nat) -> Shape s -> Shape (m :: s)

toUnsafe : Shape s -> UnsafeShape
toUnsafe ShapeNil = MkUnsafeShape []
toUnsafe (ShapeApp m xs) = let MkUnsafeShape xs' = toUnsafe xs in
                               MkUnsafeShape (m :: xs')

fromUnsafe : UnsafeShape -> Type
fromUnsafe (MkUnsafeShape []) = Shape []
fromUnsafe (MkUnsafeShape xs) = Shape (fromList xs)

transpose : Shape s1 -> Shape s2
transpose s = ?tranpose -- fromUnsafe (transposeUnsafe (toUnsafe s))


--
-- Ejemplos
--
-- toUnsafe (ShapeApp 28 (ShapeApp 14 ShapeNil))

--
-- Tensor
--
data Tensor : a -> (Shape s) -> Type where
  MkTensor : a -> (shape : Shape s) -> Tensor a shape

--
-- Tensor operations
--
using (x : a, y : a)
  t_add : (Tensor a s) -> (Tensor a s) -> (Tensor a s)
  t_add (MkTensor x s1) (MkTensor y s1) = MkTensor y s1

t_transpose : (tensor : Tensor a shape_orig) -> (Tensor a shape_target)

shapeMul : (Shape s1) -> (Shape s2) -> (Shape s3)
shapeMul x y = ?shapeMul_rhs_1
shapeMul (ShapeApp n xs) (ShapeApp m ys) = ?shapeMul_rhs_5

using (x : a, y : a)
  t_mul : (Tensor a s1) -> (Tensor a s2) -> (Tensor a (shapeMul s1 s2))
  t_mul x y = ?t_mul_rhs
