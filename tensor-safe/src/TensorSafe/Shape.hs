{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module TensorSafe.Shape where

import           Data.Singletons
import           TensorSafe.Core

#if MIN_VERSION_base(4, 11, 0)
import           GHC.TypeLits    hiding (natVal)
#else
import           GHC.TypeLits
#endif

-- | The current shapes we accept.
--   at the moment this is just one, two, and three dimensional
--   Vectors/Matricies.
--
--   These are only used with DataKinds, as Kind `Shape`, with Types 'D1, 'D2, 'D3.
data Shape
    = D1 Nat
    -- ^ One dimensional vector
    | D2 Nat Nat
    -- ^ Two dimensional matrix. Row, Column.
    | D3 Nat Nat Nat
    -- ^ Three dimensional matrix. Row, Column, Channels.

-- | Concrete data structures for a Shape.
--
--   All shapes are held in contiguous memory.
--   3D is held in a matrix (usually row oriented) which has height depth * rows.
data S (n :: Shape) where
    S1D :: ( KnownNat len )
        => R len
        -> S ('D1 len)

    S2D :: ( KnownNat rows, KnownNat columns )
        => L rows columns
        -> S ('D2 rows columns)

    S3D :: ( KnownNat rows
            , KnownNat columns
            , KnownNat depth
            , KnownNat (NatMult rows depth))
        => L (NatMult rows depth) columns
        -> S ('D3 rows columns depth)

deriving instance Show (S n)

-- Singleton instances.
--
-- These could probably be derived with template haskell, but this seems
-- clear and makes adding the KnownNat constraints simple.
-- We can also keep our code TH free, which is great.
data instance Sing (n :: Shape) where
    D1Sing :: Sing a -> Sing ('D1 a)
    D2Sing :: Sing a -> Sing b -> Sing ('D2 a b)
    D3Sing :: KnownNat (NatMult a c) => Sing a -> Sing b -> Sing c -> Sing ('D3 a b c)

instance KnownNat a => SingI ('D1 a) where
    sing = D1Sing sing

instance (KnownNat a, KnownNat b) => SingI ('D2 a b) where
    sing = D2Sing sing sing

instance (KnownNat a, KnownNat b, KnownNat c, KnownNat (NatMult a c)) => SingI ('D3 a b c) where
    sing = D3Sing sing sing sing
