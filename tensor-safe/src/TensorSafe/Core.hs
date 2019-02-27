{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}

{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TensorSafe.Core where

import           GHC.TypeLits

--
-- Natural number operations helpers
--
type family NatMult (a :: Nat) (b :: Nat) :: Nat where
    NatMult a 0 = 0
    NatMult a b = a + a + NatMult a (b - 1)


type family ShapeProduct (s :: [Nat]) :: Nat
type instance ShapeProduct '[] = 1
type instance ShapeProduct (m ': s) = NatMult m (ShapeProduct s)


data R (n :: Nat) where
    R :: (KnownNat n) => R n

instance Show (R n) where
    show = Prelude.show

data L (m :: Nat) (n :: Nat) where
    L :: (KnownNat m, KnownNat n) => L m n

instance Show (L m n) where
    show = Prelude.show
