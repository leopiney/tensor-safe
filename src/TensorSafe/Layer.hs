{-| This module defines the Layer class from which all Layers should have instances of. -}
module TensorSafe.Layer (
    InputShape,
    Layer,
    compile,
    layer,
    DLayer (..),
    CNetwork (..)
) where

import           Data.Map
import           Data.Maybe              ()

-- | Auxiliary data representation of Layers
-- IMPORTANT: If you add new Layers definitions to `TensorSafe.Layers`, you should add
-- the corresponding data structure here for the same layer.
data DLayer = DActivation
            | DAdd
            | DBatchNormalization
            | DConcatenate
            | DConv2D
            | DDense
            | DDropout
            | DFlatten
            | DGlobalAvgPooling2D
            | DInput
            | DLSTM
            | DMaxPooling
            | DRelu
            | DSoftmax
            | DUpSampling
            | DZeroPadding2D
            deriving Show

-- | Defines the skeleton of a INetwork
data CNetwork =
    CNSequence CNetwork
              | CNAdd CNetwork CNetwork
              | CNCons CNetwork CNetwork
              | CNLayer DLayer (Map String String)
              | CNReturn  -- End of initial sequence network
              | CNNil     -- End of possible nested sequence networks
              deriving Show

-- | Auxiliary type for Input Shape parameter
type InputShape = Maybe String

-- | Defines that a type is a Layer
--   Each layer can be compilated into a specific CNetwork expression which can later be used
--   to generate code to a specific backend.
class Layer x where
    -- | The layer type
    layer :: x

    -- | Given the layer and a optional inputShape generates a CNetwork structure
    compile :: x -> InputShape -> CNetwork

    {-# MINIMAL compile, layer #-}
