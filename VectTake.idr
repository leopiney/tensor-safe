module VectTake

import Data.Vect

total vectTake : (k: Fin (S n)) -> Vect n a -> Vect (finToNat k) a
vectTake FZ xs = []
vectTake (FS i) (x :: xs) = x :: vectTake i xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just idx) => Just ((index idx xs) + (index idx ys))
