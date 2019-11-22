{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-| This module implements the MNIST examples using Convs and Dense layers. -}
module TensorSafe.Examples.MnistExample (
    mnist,
    mnistDense
) where

import           TensorSafe.Layers
import           TensorSafe.Network (MkINetwork, mkINetwork)
import           TensorSafe.Shape


type MNIST = MkINetwork
    '[
        Conv2D 1 16 3 3 1 1,
        Relu,
        MaxPooling 2 2 2 2,
        Conv2D 16 32 3 3 1 1,
        Relu,
        MaxPooling 2 2 2 2,
        Conv2D 32 32 3 3 1 1,
        Relu,
        Flatten,
        Dense 64,
        Sigmoid,
        Dense 10,
        Softmax
    ]
    ('D3 28 28 1)    -- Input
    ('D1 10)       -- Output

-- | MNIST implementation using Convolutional layers
mnist :: MNIST
mnist = mkINetwork


type MNISTDense = MkINetwork
    '[
        Flatten,
        Dense 42,
        Relu,
        Dense 10,
        Sigmoid
    ]
    ('D3 28 28 1)  -- Input
    ('D1 10)       -- Output

-- | MNIST implementation using just Dense layers
mnistDense :: MNISTDense
mnistDense = mkINetwork


