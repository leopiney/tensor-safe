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
        Conv2D 1 10 5 5 1 1,
        MaxPooling 2 2 2 2,
        Relu,
        Dropout 50 1,
        Conv2D 10 16 5 5 1 1,
        MaxPooling 2 2 2 2,
        Flatten,
        Relu,
        DenseSigmoid 256 80,
        DenseSigmoid 80 10
    ]
    ('D2 28 28)    -- Input
    ('D1 10)       -- Output

mnist :: MNIST
mnist = mkINetwork
