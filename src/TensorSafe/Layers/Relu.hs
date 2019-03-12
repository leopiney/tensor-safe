{-# LANGUAGE OverloadedStrings #-}
module TensorSafe.Layers.Relu (Relu) where

import           TensorSafe.Layer

-- | TODO
data Relu = Relu deriving Show

instance Layer Relu where
    layer = Relu
    compile _ = "model.add(tf.layers.reLU())"
