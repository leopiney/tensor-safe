{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module declares the Dropout layer data type.
module TensorSafe.Layers.Dropout (Dropout) where

import Data.Kind (Type)
import Data.Map (fromList)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import TensorSafe.Compile.Expr
  ( CNetwork (CNLayer),
    DLayer (DDropout),
  )
import TensorSafe.Layer (Layer (..))

-- | A Dropout layer with rate and seed arguments
data Dropout :: Nat -> Nat -> Type where
  Dropout :: Dropout rate seed
  deriving (Show)

instance (KnownNat rate, KnownNat seed) => Layer (Dropout rate seed) where
  layer = Dropout
  compile _ _ =
    let rate = show $ natVal (Proxy :: Proxy rate)
        seed = show $ natVal (Proxy :: Proxy seed)
     in CNLayer
          DDropout
          ( fromList
              [ ("rate", rate),
                ("seed", seed)
              ]
          )
