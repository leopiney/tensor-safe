module Shape 

import Data.Vect

data UnsafeShape = MkUnsafeShape (List Nat)

Eq UnsafeShape where
  (MkUnsafeShape xs) == (MkUnsafeShape ys) = xs == ys

infixr 10 ~~>

data Shape : (dims : Vect range Nat) -> Type where
  Nil: Shape []
  (~~>) : (m : Nat) -> Shape s -> Shape (m :: s)

toUnsafe : Shape s -> UnsafeShape
toUnsafe [] = MkUnsafeShape []
toUnsafe (m ~~> xs) = let MkUnsafeShape xs' = toUnsafe xs in
                          MkUnsafeShape (m :: xs')

-- Deal with unification issues
fromUnsafe : List Nat -> Shape s
fromUnsafe { s = [] } [] = Nil
fromUnsafe { s = m :: s1 } (x :: xs) = m ~~> (fromUnsafe xs)


interface MkShape (s : Vect n Nat) where
  mkShape : Shape s

MkShape [] where
  mkShape = []

(MkShape s) => MkShape (m :: s) where
  mkShape = ?mkShapeHole


transpose : Shape s -> Shape (reverse s)
transpose [] = []
transpose (m ~~> xs) = ?transpose_rhs_1

--
-- Define Tensor data
--
-- data Tensor : a -> (Shape s) -> Type where
--   MkTensor : a -> (shape : Shape s) -> Tensor a shape
-- 
-- t_add : Tensor a s -> Tensor a s -> Tensor a s
-- t_add (MkTensor x s1) (MkTensor y s1) = MkTensor y s1
