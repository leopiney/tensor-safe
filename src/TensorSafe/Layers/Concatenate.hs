{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

-- | This module declares the Concatenate layer data type.
module TensorSafe.Layers.Concatenate (Concatenate) where

import Data.Kind (Type)
import Data.Map (empty)
import TensorSafe.Compile.Expr
  ( CNetwork (CNLayer),
    DLayer (DConcatenate),
  )
import TensorSafe.Layer (Layer (..))

-- | Concatenates two flat outputs into a new flat vector
data Concatenate :: ls1 -> ls2 -> Type where
  Concatenate :: Concatenate ls1 ls2
  deriving (Show)

-- instance (Layer l1, Layer l2) => Layer (Concatenate l1 l2) where
instance Layer (Concatenate ls1 ls2) where
  layer = Concatenate
  compile _ _ = CNLayer DConcatenate empty
