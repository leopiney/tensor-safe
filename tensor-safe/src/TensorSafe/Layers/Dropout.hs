{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module TensorSafe.Layers.Dropout (Dropout) where


import           Data.Kind        (Type)
import           GHC.TypeLits

import           TensorSafe.Layer

data Dropout :: Nat -> Type where
    Dropout :: Dropout drop
    deriving Show

instance KnownNat d => Layer (Dropout d) where
    layer = Dropout
    compile d = "model.add(tf.layers.Dropout(" ++ show (natVal d) ++ "))"
