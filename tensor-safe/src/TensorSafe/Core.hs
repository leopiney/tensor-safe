{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TensorSafe.Core ( ShapeProduct ) where

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
