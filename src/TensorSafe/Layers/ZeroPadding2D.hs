{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module declares the ZeroPadding2D layer data type.
module TensorSafe.Layers.ZeroPadding2D (ZeroPadding2D) where

import Data.Kind (Type)
import Data.Map (fromList)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import TensorSafe.Compile.Expr
  ( CNetwork (CNLayer),
    DLayer (DZeroPadding2D),
  )
import TensorSafe.Layer (Layer (..))

-- | A ZeroPadding2D layer with padding_rows and padding_cols arguments
data ZeroPadding2D :: Nat -> Nat -> Type where
  ZeroPadding2D :: ZeroPadding2D padding_rows padding_cols
  deriving (Show)

instance
  ( KnownNat padding_rows,
    KnownNat padding_cols
  ) =>
  Layer (ZeroPadding2D padding_rows padding_cols)
  where
  layer = ZeroPadding2D
  compile _ _ =
    let padding_rows = show $ natVal (Proxy :: Proxy padding_rows)
        padding_cols = show $ natVal (Proxy :: Proxy padding_cols)
     in CNLayer
          DZeroPadding2D
          ( fromList
              [ ("padding_rows", padding_rows),
                ("padding_cols", padding_cols)
              ]
          )
