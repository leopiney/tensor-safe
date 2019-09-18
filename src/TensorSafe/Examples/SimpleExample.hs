{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-| This module implements a very simple example of a deep neural network. -}
module TensorSafe.Examples.SimpleExample (
    myNet,
    myNet2,
    myNet3,
    lstm
) where


import           TensorSafe        (MkINetwork, mkINetwork)
import           TensorSafe.Layers
import           TensorSafe.Shape

type MyNet = MkINetwork '[ Sigmoid, Flatten, Relu, Flatten ] ('D2 28 28) ('D1 784)

-- | Simple network example
myNet :: MyNet
myNet = mkINetwork

type MyNet2 = MkINetwork '[ Sigmoid, Flatten, Dense 80, Relu, Flatten ] ('D2 28 28) ('D1 80)

-- | Simple network example
myNet2 :: MyNet2
myNet2 = mkINetwork

-- | Simple network example
myNet3 :: MkINetwork
    '[
        MaxPooling 2 2 2 2,
        Flatten,
        Dense 10,
        Sigmoid,
        Relu
    ]
    ('D2 28 28)
    ('D1 10)
myNet3 = mkINetwork

type MyLSTM = MkINetwork '[LSTM 8 'True] ('D2 10 20) ('D2 10 8)

-- | Simple LSTM network example
lstm :: MyLSTM
lstm = mkINetwork
