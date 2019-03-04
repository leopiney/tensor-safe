{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module TensorSafe.Layers.Dense where

import           GHC.TypeLits

import           TensorSafe.Core
import           TensorSafe.Layers
import           TensorSafe.Shape


-- | TODO
data Dense (i :: Nat) (o :: Nat) = Dense
                        !(Dense' i o)
                        !(Dense' i o)

data Dense' (i :: Nat) (o :: Nat) = Dense'
                         !(R o)   -- Bias
                         !(L o i) -- Activations

instance (KnownNat i, KnownNat o) => Show (Dense' i o) where
  show (Dense' o i) = "(" ++ show i ++ ", " ++ show o ++ ")"

instance (KnownNat i, KnownNat o) => Show (Dense i o) where
  show (Dense o i) = "Dense " ++ show i ++ " " ++ show o

-- | TODO
dummyDenseLayer :: (KnownNat i, KnownNat o) => Dense i o
dummyDenseLayer = let
    wB = R
    wN = L
    bm = R
    mm = L in
    Dense (Dense' wB wN) (Dense' bm mm)

instance (KnownNat i, KnownNat o) => LayerComponent (Dense i o) where
    layer = dummyDenseLayer

instance (KnownNat i, KnownNat o) => Layer (Dense i o) ('D1 i) ('D1 o)
