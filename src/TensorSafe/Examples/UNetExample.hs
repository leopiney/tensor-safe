{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-| This module implements the UNet model using Convs with BatchNormalization.
    This implementation is based on the Keras application implementation:
    https://github.com/zhixuhao/unet/blob/master/model.py
-}
module TensorSafe.Examples.UNetExample where

import           TensorSafe.Layers
-- import           TensorSafe.Network (MkINetwork, mkINetwork)
import           TensorSafe.Network
import           TensorSafe.Shape


type ConvBlock channels kernel_size stride_size filters1 filters2 filters3 =
    '[ Conv2D channels filters1 1 1 stride_size stride_size
     , BatchNormalization 3 99 1
     , Relu
     , Conv2D filters1 filters2 kernel_size kernel_size 1 1
     , ZeroPadding2D 1 1
     , BatchNormalization 3 99 1
     , Relu
     , Conv2D filters2 filters3 1 1 1 1
     , BatchNormalization 3 99 1
     ]

type UNet img_size channels =
    MkINetwork
    '[ Input

     , Conv2D 3 64 3 3 1 1
     , Relu

     , Conv2D 64 64 3 3 1 1
     , Relu

     , MaxPooling 2 2 2 2
    --
    --
    --
     , Conv2D 64 128 3 3 1 1
     , Relu

     , Conv2D 128 128 3 3 1 1
     , ZeroPadding2D 1 1
     , Relu

     , MaxPooling 2 2 2 2
    --
    --
    --
     , Conv2D 128 256 3 3 1 1
     , Relu

     , Conv2D 256 256 3 3 1 1
     , Relu

     , MaxPooling 2 2 2 2
    --
    --
    --
    , Conv2D 256 512 3 3 1 1
     , Relu

     , Conv2D 512 512 3 3 1 1
     , ZeroPadding2D 1 1
     , Relu

     , MaxPooling 2 2 2 2
    --
    --
    --
    , Conv2D 512 1024 3 3 1 1
     , Relu
     , Conv2D 1024 1024 3 3 1 1
     , Relu
    --
    --
    --
    -- , UpSampling 2 2
    -- , Conv2D 1024 512 2 2 1 1

    ]
    ('D3 img_size img_size channels)    -- Input
    ('D1 1024)                            -- Output

-- unet :: UNet 200 3
-- unet = mkINetwork

