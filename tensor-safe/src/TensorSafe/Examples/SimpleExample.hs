{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TensorSafe.Examples.SimpleExample where


import           TensorSafe.Layers.Dense
import           TensorSafe.Layers.Flatten
import           TensorSafe.Layers.MaxPooling
import           TensorSafe.Layers.Relu
import           TensorSafe.Layers.Sigmoid
import           TensorSafe.Network           (validNetwork)
import           TensorSafe.Shape
import           TensorSafe.Types.Network     (MkValidINetwork)


type MyNet = MkValidINetwork '[ Sigmoid, Flatten, Relu, Flatten ] ('D2 28 28) ('D1 784)
myNet :: MyNet
myNet = validNetwork

type MyNet2 = MkValidINetwork '[ Sigmoid, Flatten, Dense 784 80, Relu, Flatten ] ('D2 28 28) ('D1 80)
myNet2 :: MyNet2
myNet2 = validNetwork

myNet3 :: MkValidINetwork
       '[
          MaxPooling 2 2 2 2,
          Flatten,
          Dense 196 10,
          Sigmoid,
          Relu
        ]
        ('D2 28 28)
        ('D1 10)
myNet3 = validNetwork
