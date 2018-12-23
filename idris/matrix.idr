module Matrix

import Data.Vect

data Shape : (dims : Vect range Nat) -> Type where
  Nil : Shape []
  Constant : (n : Nat) -> Shape [n]
  Matrix : (n : Nat) -> (m : Nat) -> Shape [n, m]
  Tensor3 : (n : Nat) -> (m : Nat) -> (o : Nat) -> Shape [n, m, o]

shape_add : Shape s -> Shape s -> Shape s
shape_add [] [] = []
shape_add (Constant n) (Constant n) = Constant n
shape_add (Matrix n m) (Matrix n m) = Matrix n m
shape_add (Tensor3 n m o) (Tensor3 n m o) = Tensor3 n m o

shape_transpose : Shape s1 -> Shape s2
shape_transpose [] = []
shape_transpose (Constant n) = ?shape_transpose_rhs_2
shape_transpose (Matrix n m) = ?shape_transpose_rhs_3
shape_transpose (Tensor3 n m o) = ?shape_transpose_rhs_4
