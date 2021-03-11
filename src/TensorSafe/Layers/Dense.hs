{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module declares the Dense, a.k.a. FullyConnected, layer data type.
module TensorSafe.Layers.Dense where

import Data.Kind (Type)
import Data.Map (fromList)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import TensorSafe.Compile.Expr
  ( CNetwork (CNLayer),
    DLayer (DDense),
  )
import TensorSafe.Layer (Layer (..))

-- | A classic Dense, or FullyConnected, layer with input and output parameters.
data Dense :: Nat -> Nat -> Type where
  Dense :: Dense input output
  deriving (Show)

instance (KnownNat input, KnownNat output) => Layer (Dense input output) where
  layer = Dense
  compile _ _ =
    let input = show $ natVal (Proxy :: Proxy input)
        output = show $ natVal (Proxy :: Proxy output)
     in CNLayer
          DDense
          ( fromList
              [ ("inputDim", input),
                ("units", output)
              ]
          )
