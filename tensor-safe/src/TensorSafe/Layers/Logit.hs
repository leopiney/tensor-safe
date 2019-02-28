{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module TensorSafe.Layers.Logit (Logit) where

import           Data.Singletons
import           TensorSafe.Layers


data Logit = Logit deriving Show

instance LayerComponent Logit where
    layer = Logit

instance (SingI a) => Layer Logit a a
