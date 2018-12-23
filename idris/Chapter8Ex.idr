module Chapter8Excercises

||| This function states that if you add the same value onto the
||| front of equal lists, the resulting lists are also equal
total same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf

||| This function states that if two values, x and y, are equal,
||| and two lists, xs and ys, are equal, then the two lists
||| x::xs and y::ys must also be equal.
same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl


--
-- This type defines that three values must be equal
--
data ThreeEq : a -> b -> c -> Type where
  Same : (x : a) -> ThreeEq x x x

--
-- This function states that if three Nats are equal, the successor of each
-- one is also equal
--
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z (Same z) = Same (S z)
