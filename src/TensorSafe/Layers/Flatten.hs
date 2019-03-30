{-# LANGUAGE OverloadedStrings #-}
module TensorSafe.Layers.Flatten (Flatten) where

import           Formatting
import           TensorSafe.Layer

-- | TODO
data Flatten = Flatten deriving Show

instance Layer Flatten where
    layer = Flatten
    compile _ inputShape =
        format ("model.add(tf.layers.flatten({ inputShape: " % string % " }))") inputShape

