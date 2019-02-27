{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module TensorSafe.Layers (
    LayerComponent (..),
    Layer (..)
) where

import           Data.Kind        (Type)
import           TensorSafe.Shape


-- | TODO
class LayerComponent x where
    layer :: x

    {-# MINIMAL layer #-}

-- | TODO
class LayerComponent x => Layer x (i :: Shape) (o :: Shape) where
    type Tape x i o :: Type

    -- seal :: x -> S i -> (Tape x i o, S o)
    seal :: x -> S i -> Tape x i o

    {-# MINIMAL seal #-}
