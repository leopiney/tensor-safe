{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module TensorSafe.Types.Network where

import           Data.Kind              (Type)

import           TensorSafe.Network     (INetwork)
import           TensorSafe.Shape
import           TensorSafe.Types.Layer

-- | TODO
type family MkINetworkUnsafe (layers :: [Type]) (s :: Shape) :: Type where
    MkINetworkUnsafe ls s = INetwork ls (ComposeOut ls s)

-- | TODO
type family MaybeINetwork (net :: Type) (sOut :: Shape ) (b :: Bool) :: Type where
    MaybeINetwork net sOut 'False =
        Type -- HACK: ValidateOutput should raise an exception on this case
    MaybeINetwork net sOut 'True  = net

-- | TODO
type family MkINetwork (layers :: [Type]) (sIn :: Shape) (sOut :: Shape) :: Type where
    MkINetworkUnsafe ls sIn sOut =
        MaybeINetwork (INetwork ls (ComposeOut ls sIn)) sOut (ValidateOutput ls sIn sOut)
