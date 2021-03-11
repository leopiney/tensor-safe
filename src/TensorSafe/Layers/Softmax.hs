-- | This module declares the Softmax activation layer data type.
module TensorSafe.Layers.Softmax (Softmax) where

import Data.Map (fromList)
import TensorSafe.Compile.Expr
  ( CNetwork (CNLayer),
    DLayer (DActivation),
  )
import TensorSafe.Layer (Layer (..))

-- | A Softmax activation function
data Softmax = Softmax deriving (Show)

instance Layer Softmax where
  layer = Softmax
  compile _ _ = CNLayer DActivation (fromList [("activation", "\"softmax\"")])
