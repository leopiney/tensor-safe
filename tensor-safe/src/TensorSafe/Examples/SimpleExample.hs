{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TensorSafe.Examples.SimpleExample where


import           TensorSafe.Layers.Flatten
import           TensorSafe.Layers.Logit
import           TensorSafe.Network
import           TensorSafe.Shape


-- type MyNet =
--     Network
--     '[ FullyConnected 10 28, Logit ]
--     '[ 'D1 10, 'D1 28, 'D1 28 ]
--     -- '[ 'D1 7, 'D1 28, 'D1 29 ] -- doen't work BITCHES!!!
type MyNet = Network '[Flatten, Logit] '[ 'D2 5 5, 'D1 25, 'D1 25]

myNet :: MyNet
myNet = validNetwork
