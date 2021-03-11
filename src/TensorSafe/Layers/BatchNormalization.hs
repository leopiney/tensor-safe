{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module declares the BatchNormalization layer data type.
module TensorSafe.Layers.BatchNormalization where

import Data.Kind (Type)
import Data.Map (fromList)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import TensorSafe.Compile.Expr
  ( CNetwork (CNLayer),
    DLayer (DBatchNormalization),
  )
import TensorSafe.Layer (Layer (..))

-- | A classic BatchNormalization layer with axis, momentum and epsilon parameters
data BatchNormalization :: Nat -> Nat -> Nat -> Type where
  BatchNormalization :: BatchNormalization axis momentum epsilon
  deriving (Show)

instance
  ( KnownNat axis,
    KnownNat momentum,
    KnownNat epsilon
  ) =>
  Layer (BatchNormalization axis momentum epsilon)
  where
  layer = BatchNormalization
  compile _ _ =
    let axis = show $ natVal (Proxy :: Proxy axis)
        momentum = show $ natVal (Proxy :: Proxy momentum)
        epsilon = show $ natVal (Proxy :: Proxy epsilon)
     in CNLayer
          DBatchNormalization
          ( fromList
              [ ("axis", axis),
                ("epsilon", epsilon),
                ("momentum", momentum)
              ]
          )
