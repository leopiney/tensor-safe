{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module declares what is visible to use TensorSafe as an API.
module TensorSafe
  ( JavaScript (..),
    Python (..),
    generate,
    generateFile,
    INetwork,
    MkINetwork,
    mkINetwork,
    toCNetwork,
  )
where

import TensorSafe.Compile.Expr
import TensorSafe.Network
  ( INetwork,
    MkINetwork,
    mkINetwork,
    toCNetwork,
  )
