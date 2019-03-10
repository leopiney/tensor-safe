{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module TensorSafe.Types.Layer where

import           Data.Kind             (Type)
import           TensorSafe.LayerOut   (Out)
import           TensorSafe.Shape
import           TensorSafe.Types.Core (ShapeEquals')

-- | Returns the result of applying all the layers transformation to a specific shape.
--   Given a list of layers, this returns the expected output for the computation of each layer
--   starting with the first layer transforming the Shape `s`.
--   For example, if the initial Shape is [28, 28] and the layers are [Relu, Flatten], the result
--   will be [784].
type family ComputeOut (layers :: [Type]) (s :: Shape) :: Shape where
    ComputeOut '[] s      = s
    ComputeOut (l : ls) s = ComputeOut ls (Out l s)

-- | Returns a list of shapes describing all the transformations applied to a specific shape.
--   Given a list of layers return a type with all the Shapes from the initial Shape until the
--   last one. In theory, the last Shape should be the same than the ComputeOut function applied
--   to this same parameters.
type family ComposeOut' (layers :: [Type]) (s :: Shape) :: [Shape] where
    ComposeOut' '[] s      = '[]
    ComposeOut' (l : ls) s = ((Out l s) ': (ComposeOut' ls (Out l s)))

-- | Same than ComposeOut' but the Shape list includes the initial Shape
type family ComposeOut (layers :: [Type]) (s :: Shape) :: [Shape] where
    ComposeOut '[] s = '[]
    ComposeOut ls s  = s ': (ComposeOut' ls s)

-- | Compares the layers shape computation and the expected output
type family ValidateOutput (layers :: [Type]) (sIn :: Shape) (sOut :: Shape) :: Bool where
    ValidateOutput ls sIn sOut = ShapeEquals' (ComputeOut ls sIn) sOut
