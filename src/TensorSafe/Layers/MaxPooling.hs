{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module declares the 2D MaxPooling layer data type.
module TensorSafe.Layers.MaxPooling where

import Data.Kind (Type)
import Data.Map (fromList)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import TensorSafe.Compile.Expr
  ( CNetwork (CNLayer),
    DLayer (DMaxPooling),
  )
import TensorSafe.Layer (Layer (..))

-- | A 2D MaxPooling pooling that works for D2 and D3 shapes
data MaxPooling :: Nat -> Nat -> Nat -> Nat -> Type where
  MaxPooling :: MaxPooling kernelRows kernelColumns strideRows strideColumns
  deriving (Show)

instance
  ( KnownNat kernelRows,
    KnownNat kernelColumns,
    KnownNat strideRows,
    KnownNat strideColumns
  ) =>
  Layer (MaxPooling kernelRows kernelColumns strideRows strideColumns)
  where
  layer = MaxPooling
  compile _ _ =
    let kernelRows = natVal (Proxy :: Proxy kernelRows)
        kernelColumns = natVal (Proxy :: Proxy kernelColumns)
        strideRows = natVal (Proxy :: Proxy strideRows)
        strideColumns = natVal (Proxy :: Proxy strideColumns)
     in CNLayer
          DMaxPooling
          ( fromList
              [ ("poolSize", show [kernelRows, kernelColumns]),
                ("strides", show [strideRows, strideColumns])
              ]
          )
