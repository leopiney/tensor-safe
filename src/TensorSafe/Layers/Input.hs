{-| This module declares the Input layer data type. -}
module TensorSafe.Layers.Input (Input) where

import           Data.Map

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer

-- | Inputs the dimensions of the shapes to a list of values with shape D1
data Input = Input deriving Show

instance Layer Input where
    layer = Input
    compile _ inputShape =
        let params = case inputShape of
                        Just shape -> fromList [("inputShape", shape)]
                        Nothing    -> empty
        in
            CNLayer DInput params
