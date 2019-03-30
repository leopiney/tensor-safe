{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TensorSafe (
    Backend (..),
    eval,
    INetwork,
    MkINetwork,
    mkINetwork,
    toCNetwork
) where

import           TensorSafe.Compile.Expr (Backend (..), eval)
import           TensorSafe.Network      (INetwork, MkINetwork, mkINetwork,
                                          toCNetwork)
