{-# LANGUAGE OverloadedStrings #-}
module TensorSafe.Layers.Sigmoid (Sigmoid) where

import           Data.Map

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer

-- | TODO
data Sigmoid = Sigmoid deriving Show

instance Layer Sigmoid where
    layer = Sigmoid
    compile _ _ = "model.add(tf.layers.activation({activation: 'sigmoid'}))"
    compileCNet _ _ = CNLayer "activation" (fromList [("activation", "sigmoid")])