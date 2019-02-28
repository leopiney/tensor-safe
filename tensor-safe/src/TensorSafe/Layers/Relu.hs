{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module TensorSafe.Layers.Relu (Relu) where

import           Data.Singletons
import           TensorSafe.Layers
import           TensorSafe.Shape  (S)


data Relu = Relu deriving Show

instance LayerComponent Relu where
    layer = Relu

instance (SingI a) => Layer Relu a a where
    type Tape Relu a a = S a

    seal _ a = a
