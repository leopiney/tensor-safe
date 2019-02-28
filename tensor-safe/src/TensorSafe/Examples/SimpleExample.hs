{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TensorSafe.Examples.SimpleExample where


import           TensorSafe.Layers.Logit
import           TensorSafe.Network
import           TensorSafe.Shape


-- type MyNet =
--     Network
--     '[ FullyConnected 10 28, Logit ]
--     '[ 'D1 10, 'D1 28, 'D1 28 ]
--     -- '[ 'D1 7, 'D1 28, 'D1 29 ] -- doen't work BITCHES!!!
type MyNet = Network '[Logit] '[ 'D1 1, 'D1 1]

myNet :: MyNet
myNet = validNetwork
