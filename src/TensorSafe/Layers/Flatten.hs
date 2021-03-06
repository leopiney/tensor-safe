-- | This module declares the Flatten layer data type.
module TensorSafe.Layers.Flatten (Flatten) where

import Data.Map (empty, fromList)
import TensorSafe.Compile.Expr
  ( CNetwork (CNLayer),
    DLayer (DFlatten),
  )
import TensorSafe.Layer (Layer (..))

-- | Flattens the dimensions of the shapes to a list of values with shape D1
data Flatten = Flatten deriving (Show)

instance Layer Flatten where
  layer = Flatten
  compile _ inputShape =
    let params = case inputShape of
          Just shape -> fromList [("inputShape", shape)]
          Nothing -> empty
     in CNLayer DFlatten params
