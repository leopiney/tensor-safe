{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module TensorSafe.Layers.Logit (Logit) where

import           Data.Singletons
import           TensorSafe.Layers
import           TensorSafe.Shape  (S)


data Logit = Logit deriving Show

instance LayerComponent Logit where
    layer = Logit

instance (SingI a) => Layer Logit a a where
    type Tape Logit a a = S a

    seal _ a = a

-- instance (KnownNat i) => Layer Logit ('D1 i) ('D1 i) where
--   type Tape Logit ('D1 i) ('D1 i) = S ('D1 i)

--   seal _ (S1D y) = (S1D y, S1D y)

-- instance (KnownNat i, KnownNat j) => Layer Logit ('D2 i j) ('D2 i j) where
--   type Tape Logit ('D2 i j) ('D2 i j) = S ('D2 i j)

--   seal _ (S2D y) = (S2D y, S2D y)

-- instance (KnownNat i, KnownNat j, KnownNat k) => Layer Logit ('D3 i j k) ('D3 i j k) where
--   type Tape Logit ('D3 i j k) ('D3 i j k) = S ('D3 i j k)

--   seal _ (S3D y) = (S3D y, S3D y)
