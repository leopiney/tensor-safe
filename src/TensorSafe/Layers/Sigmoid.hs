{-| This module declares the Sigmoid activation layer data type. -}
module TensorSafe.Layers.Sigmoid (Sigmoid) where

import           Data.Map

import           TensorSafe.Layer

-- | A Sigmoid activation function
data Sigmoid = Sigmoid deriving Show

instance Layer Sigmoid where
    layer = Sigmoid
    compile _ _ = CNLayer DActivation (fromList [("activation", "\"sigmoid\"")])
