{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TensorSafe (
    Backend (..),
    evalCNetwork,
    INetwork,
    MkINetwork,
    mkINetwork,
    toCNetwork
) where

import           TensorSafe.Compile.Expr (Backend (..), evalCNetwork)
import           TensorSafe.Network      (INetwork, MkINetwork, mkINetwork,
                                          toCNetwork)
