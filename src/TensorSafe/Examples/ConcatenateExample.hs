{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | This module implements the MNIST model using some Concatenate layers
module TensorSafe.Examples.ConcatenateExample
  ( mnistConcatenate,
    mnistConcatenateComplex,
  )
where

import TensorSafe.Layers
  ( Concatenate,
    Conv2D,
    Dense,
    Flatten,
    MaxPooling,
    Relu,
    Sigmoid,
  )
import TensorSafe.Network (MkINetwork, mkINetwork)
import TensorSafe.Shape (Shape (D1, D3))

type Ls1 =
  MkINetwork
    '[ Conv2D 1 32 4 4 1 1,
       Relu,
       MaxPooling 2 2 2 2,
       Flatten
     ]
    ('D3 28 28 1) -- Input
    ('D1 4608) -- Output

type Ls2 =
  MkINetwork
    '[Conv2D 1 16 8 8 1 1, Relu, MaxPooling 2 2 2 2, Flatten]
    ('D3 28 28 1) -- Input
    ('D1 1600) -- Output

type MNISTConcatenate =
  MkINetwork
    '[ Concatenate Ls1 Ls2,
       Dense 6208 1024,
       Relu,
       Dense 1024 10,
       Sigmoid
     ]
    ('D3 28 28 1) -- Input
    ('D1 10) -- Output

-- | MNIST implementation using Concatenate layer
mnistConcatenate :: MNISTConcatenate
mnistConcatenate = mkINetwork

--
--
--
type Ls211 =
  MkINetwork
    '[ Conv2D 8 32 4 4 1 1,
       Relu,
       MaxPooling 2 2 2 2,
       Flatten
     ]
    ('D3 14 14 8) -- Input
    ('D1 800) -- Output

type Ls212 =
  MkINetwork
    '[ Conv2D 8 64 8 8 1 1,
       Relu,
       MaxPooling 2 2 2 2,
       Flatten
     ]
    ('D3 14 14 8) -- Input
    ('D1 576) -- Output

type Ls21 =
  MkINetwork
    '[ Concatenate Ls211 Ls212
     ]
    ('D3 14 14 8) -- Input
    ('D1 1376) -- Output

type Ls22 =
  MkINetwork
    '[Conv2D 8 16 8 8 1 1, Relu, MaxPooling 2 2 2 2, Flatten]
    ('D3 14 14 8) -- Input
    ('D1 144) -- Output

type MNISTConcatenateComplex =
  MkINetwork
    '[ Conv2D 1 8 1 1 1 1,
       Relu,
       MaxPooling 2 2 2 2,
       Concatenate Ls21 Ls22,
       Dense 1520 512,
       Relu,
       Dense 512 10,
       Sigmoid
     ]
    ('D3 28 28 1) -- Input
    ('D1 10) -- Output

-- | MNIST implementation using Convolutional layers
mnistConcatenateComplex :: MNISTConcatenateComplex
mnistConcatenateComplex = mkINetwork