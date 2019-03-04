{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module TensorSafe.Layers.Relu (Relu) where

import           Data.Singletons
import           TensorSafe.Layers


-- | TODO
data Relu = Relu deriving Show

instance LayerComponent Relu where
    layer = Relu

-- | TODO
instance (SingI a) => Layer Relu a a
