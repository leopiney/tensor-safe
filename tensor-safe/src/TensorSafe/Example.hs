{-# LANGUAGE DataKinds #-}
module TensorSafe.Example where


import           TensorSafe.Network

type MyNet =
    Network
    '[ FullyConnected 288 80, Logit ]
    '[ 'D3 28 28 1, 'D3 28 28 1 ]
