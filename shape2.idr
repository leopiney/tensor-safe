module Shape2

import Data.Vect

data UnsafeShape = MkUnsafeShape (List Nat)

data Shape : (dims : Vect range Nat) -> Type where
  Nil: Shape []
  (::) : (m : Nat) -> Shape s -> Shape (m :: s)

u_shape : UnsafeShape
u_shape = MkUnsafeShape [1,2,3]

s_shape : Shape [1,2,3]
s_shape = [1,2,3]

data Matrix : (rows: Nat) -> (cols: Nat) -> Type where
  MkMatrix : (rows: Nat) -> (cols: Nat) -> Matrix rows cols

m_add : Matrix r c -> Matrix r c -> Matrix r c
m_add (MkMatrix r c) (MkMatrix r c) = MkMatrix r c

m_transpose : Matrix r c -> Matrix c r
m_transpose (MkMatrix r c) = MkMatrix c r
