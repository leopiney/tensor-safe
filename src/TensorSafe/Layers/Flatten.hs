{-# LANGUAGE OverloadedStrings #-}
module TensorSafe.Layers.Flatten (Flatten) where

import           Data.Map
import           Formatting

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer

-- | Flattens the dimensions of the shapes to a list of values with shape D1
data Flatten = Flatten deriving Show

instance Layer Flatten where
    layer = Flatten
    compile _ inputShape =
        format ("model.add(tf.layers.flatten({ inputShape: " % string % " }))") inputShape
    compileCNet _ inputShape =
        let params = case inputShape of
                        Just shape -> fromList [("inputShape", shape)]
                        Nothing    -> empty
        in
            CNLayer "flatten" params
