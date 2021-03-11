-- | This module declares the GlobalAvgPooling2D layer data type.
module TensorSafe.Layers.GlobalAvgPooling2D (GlobalAvgPooling2D) where

import Data.Map (empty)
import TensorSafe.Compile.Expr
  ( CNetwork (CNLayer),
    DLayer (DGlobalAvgPooling2D),
  )
import TensorSafe.Layer (Layer (..))

-- | A GlobalAvgPooling2D function
data GlobalAvgPooling2D = GlobalAvgPooling2D deriving (Show)

instance Layer GlobalAvgPooling2D where
  layer = GlobalAvgPooling2D
  compile _ _ = CNLayer DGlobalAvgPooling2D empty
