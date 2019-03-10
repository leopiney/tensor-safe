{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module TensorSafe.Examples.MnistExample where


import           TensorSafe.Layers.Conv2D
import           TensorSafe.Layers.Dense
import           TensorSafe.Layers.Flatten
import           TensorSafe.Layers.MaxPooling
import           TensorSafe.Layers.Relu
import           TensorSafe.Layers.Sigmoid
import           TensorSafe.Network           (validNetwork)
import           TensorSafe.Shape
import           TensorSafe.Types.Network     (MkValidINetwork)

type MNIST = MkValidINetwork
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
mnist = validNetwork

--
-- BEFORE
--
-- type MNIST = INetwork
--     '[
--         Conv2D 1 10 5 5 1 1,
--         MaxPooling 2 2 2 2,
--         Relu,
--         Conv2D 10 16 5 5 1 1,
--         MaxPooling 2 2 2 2,
--         Flatten,
--         Relu,
--         Dense 256 80,
--         Sigmoid,
--         Dense 80 10,
--         Sigmoid
--     ]
--     '[
--         'D2 28 28,    -- Input
--         'D3 24 24 10, -- Conv
--         'D3 12 12 10, -- Pooling
--         'D3 12 12 10, -- Relu
--         'D3 8 8 16,   -- Conv
--         'D3 4 4 16,   -- Pooling
--         'D1 256,      -- Flatten
--         'D1 256,      -- Relu
--         'D1 80,       -- Dense
--         'D1 80,       -- Sigmoid
--         'D1 10,       -- Dense
--         'D1 10        -- Sigmoid
--     ]

-- mnist :: MNIST
-- mnist = validNetwork

