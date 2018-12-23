module Shape 

import Data.Vect

data UnsafeShape = MkUnsafeShape (List Nat)

Eq UnsafeShape where
  (MkUnsafeShape xs) == (MkUnsafeShape ys) = xs == ys

infixr 10 ~~>
infixr 11 ~>>

data Shape : (dims : Vect range Nat) -> Type where
  Nil: Shape []
  (~~>) : (m : Nat) -> Shape s -> Shape (m :: s)
  (~>>) : (m : Nat) -> Shape s -> Shape (s ++ [m])

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

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
  where
    reverseProof : Vect (k + 1) elem -> Vect (S k) elem
    reverseProof {k} result = rewrite plusCommutative 1 k in result

transpose : { s: Vect n Nat } -> Shape s -> { s2: Vect n Nat } -> { auto prf : s2 = myReverse s }-> Shape s2
transpose [] {prf = Refl} = []
transpose (m ~~> xs) {prf = Refl} = ?transpose_rhs_2 (m ~>> transpose xs)

--
-- Define Tensor data
--
-- data Tensor : a -> (Shape s) -> Type where
--   MkTensor : a -> (shape : Shape s) -> Tensor a shape
-- 
-- t_add : Tensor a s -> Tensor a s -> Tensor a s
-- t_add (MkTensor x s1) (MkTensor y s1) = MkTensor y s1
