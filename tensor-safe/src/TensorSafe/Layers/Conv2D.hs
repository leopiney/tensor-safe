{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module TensorSafe.Layers.Conv2D where

import           Data.Kind        (Type)
import           Data.Typeable    (typeOf)
import           GHC.TypeLits

import           TensorSafe.Layer


data Conv2D :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Type where
    Conv2D :: Conv2D channels filters kernelRows kernelColumns strideRows strideColumns

instance ( KnownNat c
         , KnownNat f
         , KnownNat k
         , KnownNat k'
         , KnownNat s
         , KnownNat s'
         ) => Show (Conv2D c f k k' s s') where
        show = show . typeOf


instance ( KnownNat c
         , KnownNat f
         , KnownNat k'
         , KnownNat k'
         , KnownNat s
         , KnownNat s'
         ) => Layer (Conv2D c f k k' s s') where
    layer = Conv2D
    compile _ =
        "model.add(tf.layers.conv2d({kernelSize: <<kernel>>, filters: <<filters>>}));"
