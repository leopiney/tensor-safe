{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module TensorSafe.Core where

import           GHC.TypeLits

-- | Multiplies two natural numbers
type family NatMult (a :: Nat) (b :: Nat) :: Nat where
    NatMult a 0 = 0
    NatMult a b = a + NatMult a (b - 1)

-- | Multiplies three natural numbers
type family NatMult3 (a :: Nat) (b :: Nat) (c :: Nat) :: Nat where
    NatMult3 a b 0 = 0
    NatMult3 a 0 c = 0
    NatMult3 a b c = NatMult c (NatMult a b)

-- | Multiplies all numbers on a list of natural numbers
type family ShapeProduct (s :: [Nat]) :: Nat where
    ShapeProduct '[] = 1
    ShapeProduct (m ': s) = NatMult m (ShapeProduct s)

-- | Wrapper for a Nat value
data R (n :: Nat) where
    R :: (KnownNat n) => R n

instance KnownNat n => Show (R n) where
    -- show = show . typeOf
    show n = show (natVal n)

-- | Wrapper for a tuple of 2 Nat values
data L (m :: Nat) (n :: Nat) where
    L :: (KnownNat m, KnownNat n) => L m n

instance (KnownNat m, KnownNat n) => Show (L m n) where
    -- show = show . typeOf
    show n = show (natVal n)

