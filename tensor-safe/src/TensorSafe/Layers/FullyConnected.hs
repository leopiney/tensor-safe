module TensorSafe.Layers.FullyConnected where

-- -- | A basic fully connected (or inner product) neural network layer.
-- data FullyConnected (i :: Nat) (o :: Nat) = FullyConnected
--                         !(FullyConnected' i o)   -- Neuron weights
--                         !(FullyConnected' i o)   -- Neuron momentum

-- data FullyConnected' (i :: Nat) (o :: Nat) = FullyConnected'
--                          !(R o)   -- Bias
--                          !(L o i) -- Activations

-- instance Show (FullyConnected i o) where
--   show FullyConnected {} = "FullyConnected"

-- instance (KnownNat i, KnownNat o) => Layer (FullyConnected i o) ('D1 i) ('D1 o) where
--   type Tape (FullyConnected i o) ('D1 i) ('D1 o) = R i

--   -- seal (FullyConnected (FullyConnected' wB wN) _) (S1D v) = v
--   seal _ (S1D v) = v
