{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TensorSafe.Examples.SimpleExample where


import           TensorSafe.Layers.Dense
import           TensorSafe.Layers.Flatten
import           TensorSafe.Layers.Logit
import           TensorSafe.Layers.MaxPooling
import           TensorSafe.Layers.Relu
import           TensorSafe.Network
import           TensorSafe.Shape

type MyNet = Network
             '[
                 MaxPooling 2 2 2 2,
                 Flatten,
                 Dense 196 10,
                 Logit,
                 Relu
              ]
             '[
                 'D2 28 28,
                 'D2 14 14,
                 'D1 196,
                 'D1 10,
                 'D1 10,
                 'D1 10
                --  'D1 11 -- doen't work BITCHES!!!
              ]

myNet :: MyNet
myNet = validNetwork

