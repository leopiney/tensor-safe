{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module TensorSafe.Layers.LSTM where

import           Data.Kind               (Type)
import           Data.Map
import           Data.Proxy
import           GHC.TypeLits

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer


-- | A 2D Convolutional layer
data LSTM :: Nat -> Bool -> Type where
    LSTM :: LSTM units returnSequences
    deriving Show

instance (KnownNat units) => Layer (LSTM units b) where
    layer = LSTM
    compile _ _ =
        let units = show $ natVal (Proxy :: Proxy units)
            returnSequences = show $ (Proxy :: Proxy returnSequences)
        in
            CNLayer DLSTM (fromList [
                ("units", units),
                ("returnSequences", returnSequences)
            ])

