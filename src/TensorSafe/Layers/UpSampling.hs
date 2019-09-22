{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-| This module declares the 2D UpSampling layer data type. -}
module TensorSafe.Layers.UpSampling where

import           Data.Kind               (Type)
import           Data.Map
import           Data.Proxy
import           GHC.TypeLits

import           TensorSafe.Layer

-- | A 2D UpSampling pooling that works for D2 and D3 shapes
data UpSampling :: Nat -> Nat -> Type where
    UpSampling :: UpSampling rows columns
    deriving Show

instance ( KnownNat rows
         , KnownNat columns
         ) => Layer (UpSampling rows columns) where
    layer = UpSampling
    compile _ _ =
        let rows = natVal (Proxy :: Proxy rows)
            columns = natVal (Proxy :: Proxy columns)
        in
            CNLayer DUpSampling (
                fromList [
                    ("size", show [rows, columns])
                ])
