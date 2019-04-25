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
data Add :: ls1 -> ls2 -> Type where
    Add :: Add ls1 ls2
    deriving Show

-- instance (Layer l1, Layer l2) => Layer (Add l1 l2) where
instance Layer (Add ls1 ls2) where
    layer = Add
    compile _ _ = CNLayer DAdd empty
