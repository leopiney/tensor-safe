{-| This module declares the ReLu activation layer data type. -}
module TensorSafe.Layers.Relu (Relu) where

import           Data.Map

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer

-- | A ReLu activation function
data Relu = Relu deriving Show

instance Layer Relu where
    layer = Relu
    compile _ _ = CNLayer DRelu empty
