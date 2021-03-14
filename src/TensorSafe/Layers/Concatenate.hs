{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module declares the Concatenate layer data type.
module TensorSafe.Layers.Concatenate where

import Data.Kind (Type)

-- | Concatenates two valid INetwork types
data Concatenate :: Type -> Type -> Type where
  Concatenate :: in1 -> in2 -> Concatenate in1 in2
  deriving (Show)

-- |
-- | Layer instance of Concatenate defined at Network.hs module
-- |
