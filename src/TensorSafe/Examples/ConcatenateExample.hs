{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | This module implements the MNIST model using some Concatenate layers
module TensorSafe.Examples.ConcatenateExample
  ( mnistConcatenate,
  )
where

import TensorSafe.Layers
  ( Conv2D,
    Dense,
    Flatten,
    MaxPooling,
    Relu,
    Sigmoid,
  )
import TensorSafe.Network (Concatenate, MkINetwork, mkINetwork)
import TensorSafe.Shape (Shape (D1, D3))

type Ls1 =
  MkINetwork
    '[ Conv2D 1 32 4 4 1 1,
       Relu,
       MaxPooling 2 2 2 2,
       Flatten
     ]
    ('D3 64 64 1) -- Input
    ('D1 28800) -- Output

type Ls2 =
  MkINetwork
    '[Conv2D 1 16 8 8 1 1, Relu, MaxPooling 2 2 2 2, Flatten]
    ('D3 64 64 1) -- Input
    ('D1 12544) -- Output

type MNISTConcatenate =
  MkINetwork
    '[ Concatenate Ls1 Ls2,
       Dense 41344 1024,
       Relu,
       Dense 1024 10,
       Sigmoid
     ]
    ('D3 64 64 1) -- Input
    ('D1 10) -- Output

-- | MNIST implementation using Convolutional layers
mnistConcatenate :: MNISTConcatenate
mnistConcatenate = mkINetwork