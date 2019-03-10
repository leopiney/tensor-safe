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
type family MkINetwork (layers :: [Type]) (s :: Shape) :: Type where
    MkINetwork ls s = INetwork ls (ComposeOut ls s)

-- | TODO
type family MaybeValidINetwork (net :: Type) (sOut :: Shape ) (b :: Bool) :: Type where
    MaybeValidINetwork net sOut 'False =
        Type -- HACK: ValidateOutput should raise an exception on this case
    MaybeValidINetwork net sOut 'True  = net

-- | TODO
type family MkValidINetwork (layers :: [Type]) (sIn :: Shape) (sOut :: Shape) :: Type where
    MkINetwork ls sIn sOut =
        MaybeValidINetwork (INetwork ls (ComposeOut ls sIn)) sOut (ValidateOutput ls sIn sOut)
