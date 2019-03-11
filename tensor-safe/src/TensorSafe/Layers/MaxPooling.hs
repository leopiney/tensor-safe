{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module TensorSafe.Layers.MaxPooling where

import           Data.Kind        (Type)
import           Data.Typeable    (typeOf)
import           GHC.TypeLits

import           TensorSafe.Layer

-- | TODO
data MaxPooling :: Nat -> Nat -> Nat -> Nat -> Type where
    MaxPooling :: MaxPooling kernelRows kernelColumns strideRows strideColumns

instance (KnownNat k, KnownNat k', KnownNat s, KnownNat s') => Show (MaxPooling k k' s s') where
    show = show . typeOf

instance (KnownNat k, KnownNat k', KnownNat s, KnownNat s') => Layer (MaxPooling k k' s s') where
    layer = MaxPooling
    compile _ =
        "model.add(tf.layers.maxPooling2d({poolSize: <<poolsize>>, strides: <<strides>>}))"
