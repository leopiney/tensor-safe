{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TensorSafe.Examples.MnistExample where


import           TensorSafe.Layers
import           TensorSafe.Network (MkINetwork, mkINetwork)
import           TensorSafe.Shape

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

mnist :: MNIST
mnist = mkINetwork
