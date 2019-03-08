{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TensorSafe.Core where

-- import           Data.Proxy    (Proxy (..))
-- import           Data.Typeable (typeOf)
import           GHC.TypeLits


-- | Natural number operations helpers
type family NatMult (a :: Nat) (b :: Nat) :: Nat where
    NatMult a 0 = 0
    NatMult a b = a + NatMult a (b - 1)

type family NatMult3 (a :: Nat) (b :: Nat) (c :: Nat) :: Nat where
    NatMult3 a b 0 = 0
    NatMult3 a 0 c = 0
    NatMult3 a b c = NatMult c (NatMult a b)


type family ShapeProduct (s :: [Nat]) :: Nat
type instance ShapeProduct '[] = 1
type instance ShapeProduct (m ': s) = NatMult m (ShapeProduct s)

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

