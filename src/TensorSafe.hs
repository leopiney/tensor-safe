{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TensorSafe (
    JavaScript (..),
    Python (..),
    generate,
    generateFile,
    INetwork,
    MkINetwork,
    mkINetwork,
    toCNetwork
) where

import           TensorSafe.Compile.Expr
import           TensorSafe.Network      (INetwork, MkINetwork, mkINetwork,
                                          toCNetwork)
