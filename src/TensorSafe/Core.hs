{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-| This module adds some meaningfull type operations that are of use throughout all the project.
-}
module TensorSafe.Core where

import           Data.Kind    (Type)
import           GHC.TypeLits as N

-- | Multiplies all numbers on a list of natural numbers
type family ShapeProduct (s :: [Nat]) :: Nat where
    ShapeProduct '[] = 1
    ShapeProduct (m ': s) = m N.* (ShapeProduct s)

-- | Compares two types in kinds level
type family TypeEquals (s1 :: Type) (s2 :: Type) :: Bool where
    TypeEquals s s = 'True
    TypeEquals _ _ = 'False

-- | Compares two types in kinds level and raises error if they don't match
type family TypeEquals' s1 s2 :: Type where
    TypeEquals' s s = s
    TypeEquals' s1 s2 =
        TypeError ( 'Text "Couldn't match the type "
              ':<>: 'ShowType s1
              ':<>: 'Text " with type "
              ':<>: 'ShowType s2)

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


