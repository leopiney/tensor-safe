{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-| This module implements the ResNet50 model using Convs with BatchNormalization.
    This implementation is based on the Keras application implementation:
    https://github.com/keras-team/keras-applications/blob/master/keras_applications/resnet50.py
-}
module TensorSafe.Examples.ResNet50Example where

import           TensorSafe.Layers
import           TensorSafe.Network (MkINetwork, mkINetwork)
import           TensorSafe.Shape


type IdentityBlock inputNetwork axis kernel_size filters1 filters2 filters3 i o =
    MkINetwork
    '[
        Conv2D 1 filters1 1 1 1 1,
        BatchNormalization axis 99 1,
        Relu,
        Conv2D 1 filters2 kernel_size kernel_size 1 1,
        BatchNormalization axis 99 1,
        Relu,
        Conv2D 1 filters3 1 1 1 1,
        BatchNormalization axis 99 1,
        Relu,
        Add inputNetwork,
        Relu
    ]
    i    -- Input
    o    -- Output


type Shortcut axis channels filters3 stride_size i o =
    MkINetwork
    '[
        Conv2D channels filters3 1 1 stride_size stride_size,
        BatchNormalization axis 99 1
    ]
    i    -- Input
    o    -- Output


type ConvBlock shortcut axis channels kernel_size stride_size filters1 filters2 filters3 i o =
    MkINetwork
    '[
        Conv2D channels filters1 1 1 stride_size stride_size,
        BatchNormalization axis 99 1,
        Relu,
        Conv2D filters1 filters2 kernel_size kernel_size 1 1,
        BatchNormalization axis 99 1,
        Relu,
        Conv2D filters2 filters3 1 1 1 1,
        BatchNormalization axis 99 1,
        Relu,
        Add shortcut,
        Relu
    ]
    i    -- Input
    o
type ResNet50 axis img_size channels =
    MkINetwork
    '[ Input
     , ZeroPadding2D 3 3
     , Conv2D channels 64 7 7 2 2
     , BatchNormalization axis 99 1
     , Relu
     , ZeroPadding2D 1 1
     , MaxPooling 3 3 2 2
    --  conv_block(x, 3, [64, 64, 256], stage=2, block='a', strides=(1, 1))
    -- ConvBlock shortcut axis kernel_size stride_size filters1 filters2 filters3 =
     , ConvBlock
        (Shortcut axis 64 256 1 ('D3 54 54 64) ('D3 54 54 256))
        axis
        64
        3
        1
        64
        64
        256
        ('D3 56 56 64)
        ('D3 54 54 256)
    ]
    ('D3 img_size img_size channels)    -- Input
    ('D1 10)       -- Output

resnet50 :: ResNet50 3 224 1
resnet50 = mkINetwork
