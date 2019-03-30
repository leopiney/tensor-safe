{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TensorSafe.Examples.SimpleExample where


import           TensorSafe.Layers
import           TensorSafe.Network (MkINetwork, mkINetwork)
import           TensorSafe.Shape


type MyNet = MkINetwork '[ Sigmoid, Flatten, Relu, Flatten ] ('D2 28 28) ('D1 784)
myNet :: MyNet
myNet = mkINetwork

type MyNet2 = MkINetwork '[ Sigmoid, Flatten, Dense 784 80, Relu, Flatten ] ('D2 28 28) ('D1 80)
myNet2 :: MyNet2
myNet2 = mkINetwork

myNet3 :: MkINetwork
    '[
        MaxPooling 2 2 2 2,
        Flatten,
        Dense 196 10,
        Sigmoid,
        Relu
    ]
    ('D2 28 28)
    ('D1 10)
myNet3 = mkINetwork
