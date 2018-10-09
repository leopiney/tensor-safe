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

fromUnsafe : UnsafeShape -> Shape s
fromUnsafe (MkUnsafeShape xs) = ?fromUnsafe_rhs_1
