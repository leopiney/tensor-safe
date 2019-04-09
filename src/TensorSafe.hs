{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TensorSafe (
    JavaScript (..),
    generate,
    INetwork,
    MkINetwork,
    mkINetwork,
    toCNetwork
) where

import           TensorSafe.Compile.Expr (JavaScript (..), generate)
import           TensorSafe.Network      (INetwork, MkINetwork, mkINetwork,
                                          toCNetwork)
