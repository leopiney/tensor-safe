{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-| This module implements the AlexNet examples using Convs and Dense layers. -}
module TensorSafe.Examples.AlexNetExample (
    AlexNet
) where

import           TensorSafe.Layers
import           TensorSafe.Network (MkINetwork, mkINetwork)
import           TensorSafe.Shape


type AlexNet = MkINetwork
    '[
        Conv2D 3 96 11 11 4 4, Relu,
        MaxPooling 3 3 2 2, Relu,
        -- 
        Conv2D 96 256 5 5 1 1, ZeroPadding2D 2 2, Relu,
        MaxPooling 3 3 2 2, Relu,
        -- 
        Conv2D 256 384 3 3 1 1, ZeroPadding2D 1 1, Relu,
        -- 
        Conv2D 384 384 3 3 1 1, ZeroPadding2D 1 1, Relu,
        -- 
        Conv2D 384 256 3 3 1 1, ZeroPadding2D 1 1, Relu,
        -- 
        MaxPooling 3 3 2 2, Relu,
        -- 
        Flatten,
        Dense 4096, Relu,
        Dense 4096, Relu,
        Dense 1000, Relu,
        Softmax
    ]
    ('D3 227 227 3)    -- Input
    ('D1 1000)         -- Output

-- | AlexNet implementation using Convolutional layers
alex_net :: AlexNet
alex_net = mkINetwork
