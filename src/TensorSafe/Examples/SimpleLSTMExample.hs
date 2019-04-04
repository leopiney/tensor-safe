{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TensorSafe.Examples.SimpleLSTMExample where


import           TensorSafe        (MkINetwork, mkINetwork)
import           TensorSafe.Layers
import           TensorSafe.Shape


type MyLSTM = MkINetwork '[LSTM 8 'True] ('D2 10 20) ('D2 10 8)

lstm :: MyLSTM
lstm = mkINetwork


-- type Input = MkINetwork
--     '[
--         -- Embedding (output_dim=512, input_dim=10000, input_length=100),
--         Embedding 512 10000 100,
--         LSTM 32 'False
--     ]
--     ('D1 100)
--     ('D1 32)

-- type OutputAux = MkINetwork
--     '[
--         Reference Aux,
--         Dense 32 1,
--         Sigmoid,
--         Output "aux_output"
--     ]
--     ('D1 30)
--     ('D1 1)

-- type Input2 = MkINetwork '[] ('D1 5) ('D1 5)

-- type Main = MkINetwork
--     '[
--         Concatenate (Reference Aux) (Reference Input2)
--         Dense 37 64,
--         Relu,
--         Dense 64 64,
--         Relu,
--         Dense 64 64,
--         Relu,
--         Dense 64 1,
--         Sigmoid
--     ]
--     ('D1 37)
--     ('D1 1)


