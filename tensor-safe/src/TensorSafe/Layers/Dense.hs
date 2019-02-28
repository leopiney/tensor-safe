module TensorSafe.Layers.Dense where

-- -- | A basic fully connected (or inner product) neural network layer.
-- data Dense (i :: Nat) (o :: Nat) = Dense
--                         !(Dense' i o)   -- Neuron weights
--                         !(Dense' i o)   -- Neuron momentum

-- data Dense' (i :: Nat) (o :: Nat) = Dense'
--                          !(R o)   -- Bias
--                          !(L o i) -- Activations

-- instance Show (Dense i o) where
--   show Dense {} = "Dense"

-- instance (KnownNat i, KnownNat o) => Layer (Dense i o) ('D1 i) ('D1 o) where
--   type Tape (Dense i o) ('D1 i) ('D1 o) = R i

--   -- seal (Dense (Dense' wB wN) _) (S1D v) = v
--   seal _ (S1D v) = v
