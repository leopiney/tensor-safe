{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE PolyKinds #-}
{-| This module declares the Concatenate layer data type. -}
module TensorSafe.Layers.Concatenate (Concatenate) where

import           Data.Kind               (Type)
import           Data.Map

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer

-- | Concatenates the dimensions of the shapes to a list of values with shape D1
data Concatenate :: ls1 -> ls2 -> Type where
    Concatenate :: Concatenate ls1 ls2
    deriving Show

-- instance (Layer l1, Layer l2) => Layer (Concatenate l1 l2) where
instance Layer (Concatenate ls1 ls2) where
    layer = Concatenate
    compile _ _ = CNLayer DConcatenate empty
