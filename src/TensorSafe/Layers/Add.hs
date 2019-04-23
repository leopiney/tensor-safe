{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE PolyKinds #-}
{-| This module declares the Add layer data type. -}
module TensorSafe.Layers.Add (Add) where

import           Data.Kind               (Type)
import           Data.Map

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer

-- | Adds the dimensions of the shapes to a list of values with shape D1
data Add :: layer -> Type where
    Add :: Add layer
    deriving Show

instance (Layer layer) => Layer (Add layer) where
    layer = Add
    compile _ _ = CNLayer DAdd empty
