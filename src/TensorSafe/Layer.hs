module TensorSafe.Layer where

import           Data.Maybe              ()
import           Data.Text.Lazy          (Text)

import           TensorSafe.Compile.Expr

-- | Defines that a type is a Layer
--   Each layer can be compilated into a specific line for now. This is incompatible with
--   dependencies between lines of code while compiling the networks to a specific frontend.
class Layer x where
    layer :: x
    compile :: x -> String -> Text
    compileCNet :: x -> Maybe String -> CNetwork

    {-# MINIMAL compile, compileCNet, layer #-}
