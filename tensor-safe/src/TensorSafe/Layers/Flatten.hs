module TensorSafe.Layers.Flatten (Flatten) where

import           TensorSafe.Layer

-- | TODO
data Flatten = Flatten deriving Show

instance Layer Flatten where
    layer = Flatten
    compile _ = "model.add(tf.layers.flatten())"

