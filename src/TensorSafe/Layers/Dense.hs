{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-| This module declares the Dense, a.k.a. FullyConnected, layer data type. -}
module TensorSafe.Layers.Dense where

import           Data.Kind               (Type)
import           Data.Map
import           Data.Proxy
import           GHC.TypeLits

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer


-- | A classic Dense, or FullyConnected, layer with input and output parameters.
data Dense :: Nat -> Type where
    Dense :: Dense output
    deriving Show

instance (KnownNat output) => Layer (Dense output) where
    layer = Dense
    compile _ inputShape =
        let params = case inputShape of
                Just shape -> fromList [("inputShape", shape)]
                Nothing    -> empty
            output = show $ natVal (Proxy :: Proxy output)
        in
            CNLayer DDense (union params (fromList [
                ("units", output)
            ]))