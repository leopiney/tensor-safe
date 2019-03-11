{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TensorSafe.Examples.MnistExample where


import           TensorSafe.Layers.Conv2D
import           TensorSafe.Layers.Dense
import           TensorSafe.Layers.Flatten
import           TensorSafe.Layers.MaxPooling
import           TensorSafe.Layers.Relu
import           TensorSafe.Layers.Sigmoid
import           TensorSafe.Network           (mkINetwork)
import           TensorSafe.Shape
import           TensorSafe.Types.Network     (MkINetwork)

type MNIST = MkINetwork
    '[
        Conv2D 1 10 5 5 1 1,
        MaxPooling 2 2 2 2,
        Relu,
        Conv2D 10 16 5 5 1 1,
        MaxPooling 2 2 2 2,
        Flatten,
        Relu,
        Dense 256 80,
        Sigmoid,
        Dense 80 10,
        Sigmoid
    ]
    ('D2 28 28)    -- Input
    ('D1 10)        -- Output

mnist :: MNIST
mnist = mkINetwork
