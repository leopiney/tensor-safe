{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module TensorSafe.Layers.Logit (Logit) where

import           Data.Singletons
import           TensorSafe.Layers

-- | TODO
data Logit = Logit deriving Show

instance LayerComponent Logit where
    layer = Logit
    compile _ = "models.add(tf.layers.activation({activation: 'sigmoid'}))"

-- | TODO
instance (SingI a) => Layer Logit a a
