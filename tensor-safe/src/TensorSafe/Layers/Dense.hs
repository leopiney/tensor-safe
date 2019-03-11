{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module TensorSafe.Layers.Dense where

import           Data.Kind        (Type)
import           Data.Typeable    (typeOf)
import           GHC.TypeLits

import           TensorSafe.Layer


data Dense :: Nat -> Nat -> Type where
  Dense :: Dense input output

instance (KnownNat i, KnownNat o) => Show (Dense i o) where
  show = show . typeOf

instance Layer (Dense input output) where
  layer = Dense
  compile _ = "model.add(tf.layers.dense({units: <<dense>> })"
