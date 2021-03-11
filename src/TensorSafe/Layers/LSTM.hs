{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module declares the classing LSTM layer data type.
module TensorSafe.Layers.LSTM where

import Data.Kind (Type)
import Data.Map (fromList)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import TensorSafe.Compile.Expr (CNetwork (CNLayer), DLayer (DLSTM))
import TensorSafe.Layer (Layer (..))

-- | A LSTM layer with a number of units and a option to return the original sequences.
data LSTM :: Nat -> Bool -> Type where
  LSTM :: LSTM units returnSequences
  deriving (Show)

instance (KnownNat units) => Layer (LSTM units b) where
  layer = LSTM
  compile _ _ =
    let units = show $ natVal (Proxy :: Proxy units)
        returnSequences = show $ (Proxy :: Proxy returnSequences)
     in CNLayer
          DLSTM
          ( fromList
              [ ("units", units),
                ("returnSequences", returnSequences)
              ]
          )
