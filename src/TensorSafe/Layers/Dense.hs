{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module TensorSafe.Layers.Dense where

import           Data.Kind               (Type)
import           Data.Map
import           Data.Proxy
import           GHC.TypeLits

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer


data Dense :: Nat -> Nat -> Type where
    Dense :: Dense input output
    deriving Show

instance (KnownNat input, KnownNat output) => Layer (Dense input output) where
    layer = Dense
    compile _ _ =
        let input = show $ natVal (Proxy :: Proxy input)
            output = show $ natVal (Proxy :: Proxy output)
        in
            CNLayer DDense (fromList [
              ("inputDim", input),
              ("units", output)
            ])
