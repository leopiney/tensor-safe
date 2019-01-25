{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module TensorSafe.Layers where

import           GHC.TypeLits

import           TensorSafe.Core

data Conv2D :: Nat -- number of channels (depth)
            -> Nat -- number of filters
            -> Nat -- number of rows in kernel filter
            -> Nat -- number of cols in kernel filter
            -> Nat -- the row stride in the conv filter
            -> Nat -- the col stride in the conv filter
            -> * where
    Conv2D :: ( KnownNat channels
              , KnownNat filters
              , KnownNat kernelRows
              , KnownNat kernelColumns
              , KnownNat strideRows
              , KnownNat strideColumns
              , KnownNat kernelFlattened
              , kernelFlattened ~ ShapeProduct [kernelRows, kernelColumns, channels])
           => Conv2D channels filters kernelRows kernelColumns strideRows strideColumns
