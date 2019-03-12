{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module TensorSafe.Layers.MaxPooling where

import           Data.Kind        (Type)
import           Data.Proxy
import           Data.Typeable    (typeOf)
import           Formatting
import           GHC.TypeLits

import           TensorSafe.Layer

-- | TODO
data MaxPooling :: Nat -> Nat -> Nat -> Nat -> Type where
    MaxPooling :: MaxPooling kernelRows kernelColumns strideRows strideColumns

instance (KnownNat k, KnownNat k', KnownNat s, KnownNat s') => Show (MaxPooling k k' s s') where
    show = show . typeOf

instance ( KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         ) => Layer (MaxPooling kernelRows kernelColumns strideRows strideColumns) where
    layer = MaxPooling
    compile _ =
        let kernelRows = show $ natVal (Proxy :: Proxy kernelRows)
            kernelColumns = show $ natVal (Proxy :: Proxy kernelColumns)
            strideRows = show $ natVal (Proxy :: Proxy strideRows)
            strideColumns = show $ natVal (Proxy :: Proxy strideColumns)
        in
        format ("model.add(tf.layers.maxPooling2d({poolSize: [" % string % ", " % string % "], strides: [" % string % ", " % string % "]}))") kernelRows kernelColumns strideRows strideColumns
