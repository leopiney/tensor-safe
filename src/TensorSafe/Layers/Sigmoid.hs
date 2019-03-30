{-# LANGUAGE OverloadedStrings #-}
module TensorSafe.Layers.Sigmoid (Sigmoid) where

import           TensorSafe.Layer

-- | TODO
data Sigmoid = Sigmoid deriving Show

instance Layer Sigmoid where
    layer = Sigmoid
    compile _ _ = "model.add(tf.layers.activation({activation: 'sigmoid'}))"
