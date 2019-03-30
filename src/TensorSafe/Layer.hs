module TensorSafe.Layer where

import           Data.Text.Lazy (Text)

-- | Defines that a type is a Layer
--   Each layer can be compilated into a specific line for now. This is incompatible with
--   dependencies between lines of code while compiling the networks to a specific frontend.
class Layer x where
    layer :: x
    compile :: x -> String -> Text

    {-# MINIMAL compile, layer #-}
