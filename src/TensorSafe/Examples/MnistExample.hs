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

type DenseRelu i o =
    MkINetwork '[ Dense i o, Relu ] ('D1 i) ('D1 o)

type DenseSigmoid i o =
    MkINetwork '[ Dense i o, Sigmoid ] ('D1 i) ('D1 o)

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
        DenseSigmoid 288 64,
        DenseSigmoid 64 10
    ]
    ('D3 28 28 1)    -- Input
    ('D1 10)       -- Output

-- | MNIST implementation using Convolutional layers
mnist :: MNIST
mnist = mkINetwork


type MNIST2 n = MkINetwork
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
        DenseSigmoid 288 64,
        DenseSigmoid 64 10
    ]
    ('D3 n n 1)    -- Input
    ('D1 10)       -- Output


mnist2 :: MNIST2 28
mnist2 = mkINetwork


type MNISTDense = MkINetwork
    '[
        Flatten,
        Dense 784 42,
        Relu,
        Dense 42 10,
        Sigmoid
    ]
    ('D3 28 28 1)  -- Input
    ('D1 10)       -- Output

-- | MNIST implementation using just Dense layers
mnistDense :: MNISTDense
mnistDense = mkINetwork
