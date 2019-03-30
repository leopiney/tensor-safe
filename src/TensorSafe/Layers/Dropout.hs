{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module TensorSafe.Layers.Dropout (Dropout) where

import           Data.Kind               (Type)
import           Data.Map
import           Data.Proxy
import           Formatting
import           GHC.TypeLits

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer

data Dropout :: Nat -> Nat -> Type where
    Dropout :: Dropout rate seed
    deriving Show

instance (KnownNat rate, KnownNat seed) => Layer (Dropout rate seed) where
    layer = Dropout
    compile _ _ =
        let rate = show $ natVal (Proxy :: Proxy rate)
            seed = show $ natVal (Proxy :: Proxy seed)
        in format ("model.add(tf.layers.dropout({rate: " % string % ", seed: " % string % "}))") rate seed
    compileCNet _ _ =
        let rate = show $ natVal (Proxy :: Proxy rate)
            seed = show $ natVal (Proxy :: Proxy seed)
        in
            CNLayer "dropout" (fromList [
                ("rate", rate),
                ("seed", seed)
            ])
