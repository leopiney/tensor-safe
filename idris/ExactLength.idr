module ExactLength

data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

--
-- First attempt fails
--
-- exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
-- exactLength {m} len input = case m == len of
--                                  False => Nothing
--                                  True => Just ?exactLength_rhs_2 -- Just input fails!

--
-- This is a proof that two numbers are equals
--
-- data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
--   Same : (num : Nat) -> EqNat num num
-- 
-- sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
-- sameS k k (Same k) = Same (S k)
-- 
-- checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
-- checkEqNat Z Z = Just (Same Z) 
-- checkEqNat Z (S k) = Nothing
-- checkEqNat (S k) Z = Nothing
-- checkEqNat (S k) (S j) = case checkEqNat k j of
--                               Nothing => Nothing
--                               (Just eq) => Just (sameS _ _ eq)

-- exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
-- exactLength {m} len input = case checkEqNat m len of
--                                  Nothing => Nothing
--                                  Just (Same len) => Just input

--
-- This is a proof that two numbers are equals using the (=) type
--
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z = Just Refl
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              (Just prf) => Just (cong prf)

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Nothing => Nothing
                                 (Just Refl) => Just input
