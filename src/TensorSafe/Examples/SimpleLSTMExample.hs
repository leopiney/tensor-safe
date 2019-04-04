{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TensorSafe.Examples.SimpleLSTMExample where


import           TensorSafe        (MkINetwork, mkINetwork)
import           TensorSafe.Layers
import           TensorSafe.Shape


type MyLSTM = MkINetwork '[LSTM 8 'True] ('D2 10 20) ('D2 10 8)

lstm :: MyLSTM
lstm = mkINetwork
