{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module TensorSafe.Layers (
    LayerComponent (..),
    Layer
) where

import           TensorSafe.Shape

-- | TODO
class LayerComponent x where
    layer :: x
    compile :: x -> String

    {-# MINIMAL compile, layer #-}

-- | TODO
class LayerComponent x => Layer x (i :: Shape) (o :: Shape)
