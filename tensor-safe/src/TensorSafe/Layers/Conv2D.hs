{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module TensorSafe.Layers.Conv2D where

import           Data.Kind         (Type)
import           GHC.TypeLits

import           TensorSafe.Core
import           TensorSafe.Layers
import           TensorSafe.Shape


data Conv2D :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Type where
    Conv2D :: Conv2D channels filters kernelRows kernelColumns strideRows strideColumns

instance Show (Conv2D c f k k' s s') where
    show Conv2D = "Conv2D"


instance ( KnownNat c
         , KnownNat f
         , KnownNat k'
         , KnownNat k'
         , KnownNat s
         , KnownNat s'
         ) => LayerComponent (Conv2D c f k k' s s') where
    layer = Conv2D


instance ( KnownNat channels
         , KnownNat filters
         , KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         , KnownNat inputRows
         , KnownNat inputColumns
         , KnownNat outputRows
         , KnownNat outputColumns
         , (NatMult (outputRows - 1) strideRows) ~ (inputRows - kernelRows)
         , (NatMult (outputColumns - 1) strideColumns) ~ (inputColumns - kernelColumns)
         ) => Layer
              (Conv2D channels filters kernelRows kernelColumns strideRows strideColumns)
              ('D3 inputRows inputColumns channels)
              ('D3 outputRows outputColumns filters) where
    type Tape
         (Conv2D channels filters kernelRows kernelColumns strideRows strideColumns)
         ('D3 inputRows inputColumns channels)
         ('D3 outputRows outputColumns filters) = ()

    seal _ _ = ()

instance ( KnownNat filters
         , KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         , KnownNat inputRows
         , KnownNat inputColumns
         , KnownNat outputRows
         , KnownNat outputColumns
         , (NatMult (outputRows - 1) strideRows) ~ (inputRows - kernelRows)
         , (NatMult (outputColumns - 1) strideColumns) ~ (inputColumns - kernelColumns)
         ) => Layer
              (Conv2D 1 filters kernelRows kernelColumns strideRows strideColumns)
              ('D2 inputRows inputColumns)
              ('D3 outputRows outputColumns filters) where
    type Tape
         (Conv2D 1 filters kernelRows kernelColumns strideRows strideColumns)
         ('D2 inputRows inputColumns)
         ('D3 outputRows outputColumns filters) = ()

    seal _ _ = ()

instance ( KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         , KnownNat inputRows
         , KnownNat inputColumns
         , KnownNat outputRows
         , KnownNat outputColumns
         , (NatMult (outputRows - 1) strideRows) ~ (inputRows - kernelRows)
         , (NatMult (outputColumns - 1) strideColumns) ~ (inputColumns - kernelColumns)
         ) => Layer
              (Conv2D 1 1 kernelRows kernelColumns strideRows strideColumns)
              ('D2 inputRows inputColumns)
              ('D2 outputRows outputColumns) where
    type Tape
         (Conv2D 1 1 kernelRows kernelColumns strideRows strideColumns)
         ('D2 inputRows inputColumns)
         ('D2 outputRows outputColumns) = ()

    seal _ _ = ()

instance ( KnownNat channels
         , KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         , KnownNat inputRows
         , KnownNat inputColumns
         , KnownNat outputRows
         , KnownNat outputColumns
         , (NatMult (outputRows - 1) strideRows) ~ (inputRows - kernelRows)
         , (NatMult (outputColumns - 1) strideColumns) ~ (inputColumns - kernelColumns)
         ) => Layer
              (Conv2D channels 1 kernelRows kernelColumns strideRows strideColumns)
              ('D3 inputRows inputColumns channels)
              ('D2 outputRows outputColumns) where
    type Tape
         (Conv2D channels 1 kernelRows kernelColumns strideRows strideColumns)
         ('D3 inputRows inputColumns channels)
         ('D2 outputRows outputColumns) = ()

    seal _ _ = ()
