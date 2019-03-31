module TensorSafe.Layer where

import           Data.Maybe              ()

import           TensorSafe.Compile.Expr

type InputShape = Maybe String

-- | Defines that a type is a Layer
--   Each layer can be compilated into a specific CNetwork expression which can later be used
--   to generate code to a specific backend.
class Layer x where
    -- | The layer type
    layer :: x

    -- | Given the layer and a optional `inputShape` generates a CNetwork structure
    compile :: x -> InputShape -> CNetwork

    {-# MINIMAL compile, layer #-}
