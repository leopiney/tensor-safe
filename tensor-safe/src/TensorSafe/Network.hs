{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module TensorSafe.Network where

import           Data.Kind       (Type)
import           Data.Singletons

#if MIN_VERSION_base(4, 11, 0)
import           GHC.TypeLits    hiding (natVal)
#else
import           GHC.TypeLits
#endif

import           TensorSafe.Core

-- ********************************************************************************************* --
-- ********************************************************************************************* --
-- ********************************************************************************************* --
-- ********************************************************************************************* --

-- This should be a Shape module

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

-- ********************************************************************************************* --
-- ********************************************************************************************* --
-- ********************************************************************************************* --
-- ********************************************************************************************* --

-- This should be a Layer module

class Layer x (i :: Shape) (o :: Shape) where
  type Tape x i o :: Type

  seal :: x -> S i -> (Tape x i o, S o)

-- | A basic fully connected (or inner product) neural network layer.
data FullyConnected (i :: Nat) (o :: Nat) = FullyConnected
                        !(FullyConnected' i o)   -- Neuron weights
                        !(FullyConnected' i o)   -- Neuron momentum

data FullyConnected' (i :: Nat) (o :: Nat) = FullyConnected'
                         !(R o)   -- Bias
                         !(L o i) -- Activations

instance Show (FullyConnected i o) where
  show FullyConnected {} = "FullyConnected"

data Logit = Logit
  deriving Show

instance (SingI a) => Layer Logit a a where
  type Tape Logit a a = S a

  seal _ a = (a, a)

-- instance (KnownNat i) => Layer Logit ('D1 i) ('D1 i) where
--   type Tape Logit ('D1 i) ('D1 i) = S ('D1 i)

--   seal _ (S1D y) = (S1D y, S1D y)

-- instance (KnownNat i, KnownNat j) => Layer Logit ('D2 i j) ('D2 i j) where
--   type Tape Logit ('D2 i j) ('D2 i j) = S ('D2 i j)

--   seal _ (S2D y) = (S2D y, S2D y)

-- instance (KnownNat i, KnownNat j, KnownNat k) => Layer Logit ('D3 i j k) ('D3 i j k) where
--   type Tape Logit ('D3 i j k) ('D3 i j k) = S ('D3 i j k)

--   seal _ (S3D y) = (S3D y, S3D y)


-- ********************************************************************************************* --
-- ********************************************************************************************* --
-- ********************************************************************************************* --
-- ********************************************************************************************* --

-- This should be the Network module

data Network :: [Type] -> [Shape] -> Type where
    NNil  :: SingI i
            => Network '[] '[i]

    (:~~) :: (SingI i, SingI h, Layer x i h)
            => !x
            -> !(Network xs (h ': hs))
            -> Network (x ': xs) (i ': h ': hs)
infixr 5 :~~


class ValidNetwork (xs :: [Type]) (ss :: [Shape]) where
  validNetwork :: Network xs ss

  {-# MINIMAL validNetwork #-}

instance (SingI i) => ValidNetwork '[] '[i] where
  validNetwork = NNil

instance (SingI i, SingI o, Layer x i o, ValidNetwork xs (o ': rs)) => ValidNetwork (x ': xs) (i ': o ': rs) where
  validNetwork = validNetwork


type MyNet =
  Network
  '[ Logit ]
  '[ 'D1 28, 'D1 28 ]
  -- '[ 'D1 28, 'D1 29 ] -- doen't work BITCHES!!!

myNet :: MyNet
myNet = validNetwork
