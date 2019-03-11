{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module TensorSafe.LayerOut where

import           Data.Kind                    (Type)
import           GHC.TypeLits
import           GHC.TypeLits.Extra           (Div)

import           TensorSafe.Core              (NatMult, NatMult3)
import           TensorSafe.Shape

import           TensorSafe.Layers.Conv2D
import           TensorSafe.Layers.Dense
import           TensorSafe.Layers.Flatten
import           TensorSafe.Layers.MaxPooling
import           TensorSafe.Layers.Relu
import           TensorSafe.Layers.Sigmoid

-- | Defines the expected output of a layer
--   This type function should be instanciated for each of the Layers defined.
type family Out (l :: Type) (s :: Shape) :: Shape where
    --
    --
    --
    Out (Conv2D 1 1 k k' s s') ('D2 inputRows inputColumns) =
        ('D2 (1 + (Div (inputRows - k) s))
             (1 + (Div (inputColumns - k') s'))
        )

    Out (Conv2D 1 filters k k' s s') ('D2 inputRows inputColumns) =
        ('D3 (1 + (Div (inputRows - k) s))
             (1 + (Div (inputColumns - k') s'))
             filters
        )

    Out (Conv2D channels 1 k k' s s') ('D3 inputRows inputColumns channels) =
        ('D2 (1 + (Div (inputRows - k) s))
             (1 + (Div (inputColumns - k') s'))
        )

    Out (Conv2D channels filters k k' s s') ('D3 inputRows inputColumns channels) =
        ('D3 (1 + (Div (inputRows - k) s))
             (1 + (Div (inputColumns - k') s'))
             filters
        )

    --
    --
    --
    Out (Dense i o) ('D1 i) = 'D1 o

    --
    --
    --
    Out Flatten ('D1 x)     = 'D1 x
    Out Flatten ('D2 x y)   = 'D1 (NatMult x y)
    Out Flatten ('D3 x y z) = 'D1 (NatMult3 x y z)

    --
    --
    --
    Out (MaxPooling k k' s s') ('D2 inputRows inputColumns) =
        ('D2 (1 + (Div (inputRows - k) s))
             (1 + (Div (inputColumns - k') s'))
        )

    Out (MaxPooling k k' s s') ('D3 inputRows inputColumns channels) =
        ('D3 (1 + (Div (inputRows - k) s))
             (1 + (Div (inputColumns - k') s'))
             channels
        )

    --
    --
    --
    Out Relu s           = s

    --
    --
    --
    Out Sigmoid s           = s

    --
    -- Edge case or not defined raise an error
    --
    Out l sOut =
        TypeError ( 'Text "Couldn't apply the Layer \""
              ':<>: 'ShowType l
              ':<>: 'Text "\" with the output Shape \""
              ':<>: 'ShowType sOut
              ':<>: 'Text "\"")
