{-# LANGUAGE DataKinds #-}
module TensorSafe.Example where


import           TensorSafe.Network


type MyNet =
    Network
    '[ FullyConnected 10 28, Logit ]
    '[ 'D1 10, 'D1 28, 'D1 28 ]
    -- '[ 'D1 7, 'D1 28, 'D1 29 ] -- doen't work BITCHES!!!

myNet :: MyNet
myNet = validNetwork
