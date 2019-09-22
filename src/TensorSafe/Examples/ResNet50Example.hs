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


type IdentityBlock channels kernel_size filters1 filters2 filters3 =
    '[ Conv2D channels filters1 1 1 1 1
     , BatchNormalization 3 99 1
     , Relu
     , Conv2D filters1 filters2 kernel_size kernel_size 1 1
     , BatchNormalization 3 99 1
     , ZeroPadding2D 1 1
     , Relu
     , Conv2D filters2 filters3 1 1 1 1
     , BatchNormalization 3 99 1
     ]

type Shortcut channels stride_size filters3 =
    '[ Conv2D channels filters3 1 1 stride_size stride_size
     , BatchNormalization 3 99 1
     ]


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

type ResNet50 img_size channels =
    MkINetwork
    '[ Input
     , ZeroPadding2D 3 3
     , Conv2D channels 64 7 7 2 2
     , BatchNormalization 3 99 1
     , Relu
     , ZeroPadding2D 1 1
     , MaxPooling 3 3 2 2

    -- First block
     , Add (ConvBlock 64 3 1 64 64 256) (Shortcut 64 1 256) , Relu
     , Add (IdentityBlock 256 3 64 64 256) '[Input] , Relu
     , Add (IdentityBlock 256 3 64 64 256) '[Input] , Relu

    -- Second block
    --  , Add (ConvBlock 256 3 1 128 128 512) (Shortcut 256 1 512) , Relu
    --  , Add (IdentityBlock 512 3 128 128 512) '[Input] , Relu
    --  , Add (IdentityBlock 512 3 128 128 512) '[Input] , Relu
    --  , Add (IdentityBlock 512 3 128 128 512) '[Input] , Relu

    -- --  Third block
    --  , Add (ConvBlock 512 3 1 256 256 1024) (Shortcut 512 1 1024) , Relu
    --  , Add (IdentityBlock 1024 3 256 256 1024) '[Input] , Relu
    --  , Add (IdentityBlock 1024 3 256 256 1024) '[Input] , Relu
    --  , Add (IdentityBlock 1024 3 256 256 1024) '[Input] , Relu
    --  , Add (IdentityBlock 1024 3 256 256 1024) '[Input] , Relu
    --  , Add (IdentityBlock 1024 3 256 256 1024) '[Input] , Relu

    -- --  -- Fourth block
    --  , Add (ConvBlock 1024 3 1 512 512 2048) (Shortcut 1024 1 2048) , Relu
    --  , Add (IdentityBlock 2048 3 512 512 2048) '[Input] , Relu
    --  , Add (IdentityBlock 2048 3 512 512 2048) '[Input] , Relu

     , GlobalAvgPooling2D
     , Dense 1000
    ]
    ('D3 img_size img_size channels)    -- Input
    ('D1 1000)                            -- Output

resnet50 :: ResNet50 224 1
resnet50 = mkINetwork
