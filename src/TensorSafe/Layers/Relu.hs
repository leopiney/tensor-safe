{-# LANGUAGE OverloadedStrings #-}
module TensorSafe.Layers.Relu (Relu) where

import           Data.Map

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer

-- | TODO
data Relu = Relu deriving Show

instance Layer Relu where
    layer = Relu
    compile _ _ = "model.add(tf.layers.reLU())"
    compileCNet _ _ = CNLayer "reLU" empty
