{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module TensorSafe.Layers.Dense where

import           Data.Kind        (Type)
import           Data.Proxy
import           Data.Typeable    (typeOf)
import           Formatting
import           GHC.TypeLits

import           TensorSafe.Layer


data Dense :: Nat -> Nat -> Type where
  Dense :: Dense input output

instance (KnownNat i, KnownNat o) => Show (Dense i o) where
  show = show . typeOf

instance (KnownNat input, KnownNat output) => Layer (Dense input output) where
  layer = Dense
  compile _ _ =
    let input = show $ natVal (Proxy :: Proxy input)
        output = show $ natVal (Proxy :: Proxy output)
    in format ("model.add(tf.layers.dense({inputDim: " % string % ", units: " % string % " }))") input output

