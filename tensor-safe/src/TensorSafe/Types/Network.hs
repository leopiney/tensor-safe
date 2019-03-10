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

import           Data.Kind        (Type)
import           Data.Singletons
import           Data.Typeable    (typeOf)

import           GHC.TypeLits
import           TensorSafe.Core
import           TensorSafe.Shape


data Flatten = Flatten deriving Show
data Relu = Relu deriving Show
data Sigmoid = Sigmoid deriving Show


class Layer x where
    layer :: x

instance Layer Flatten where
    layer = Flatten
instance Layer Relu where
    layer = Relu
instance Layer Sigmoid where
    layer = Sigmoid

data Network :: [Type] -> Type where
    NNil  :: Network '[]

    (:~~) :: Layer x
          => !x
          -> !(Network xs)
          -> Network (x ': xs)
infixr 5 :~~


data INetwork :: [Type] -> [Shape] -> Type where
    INNil  :: SingI i
            => INetwork '[] '[i]

    (:~>) :: (SingI i, SingI h, Layer x)
            => !x
            -> !(INetwork xs (h ': hs))
            -> INetwork (x ': xs) (i ': h ': hs)
infixr 5 :~>

type family Out (l :: Type) (s :: Shape) :: Shape where
    Out Relu s              = s
    Out Sigmoid s           = s
    Out Flatten ('D1 x)     = 'D1 x
    Out Flatten ('D2 x y)   = 'D1 (NatMult x y)
    Out Flatten ('D3 x y z) = 'D1 (NatMult3 x y z)

type family ComputeOut (layers :: [Type]) (s :: Shape) :: Shape where
    ComputeOut '[] s      = s
    ComputeOut (l : ls) s = ComputeOut ls (Out l s)

type family ComposeOut' (layers :: [Type]) (s :: Shape) :: [Shape] where
    ComposeOut' '[] s      = '[]
    ComposeOut' (l : ls) s = ((Out l s) ': (ComposeOut' ls (Out l s)))

type family ComposeOut (layers :: [Type]) (s :: Shape) :: [Shape] where
    ComposeOut '[] s = '[]
    ComposeOut ls s  = s ': (ComposeOut' ls s)

type family ShapeEqual (sIn :: Shape) (sOut :: Shape) :: Bool where
    ShapeEqual s s = 'True
    ShapeEqual _ _ = 'False

type family ValidateOutput (layers :: [Type]) (sIn :: Shape) (sOut :: Shape) :: Bool where
    ValidateOutput ls sIn sOut = ShapeEqual (ComputeOut ls sIn) sOut

type family MkINetwork (layers :: [Type]) (s :: Shape) :: Type where
    MkINetwork ls s = INetwork ls (ComposeOut ls s)

type family MaybeValidINetwork (net :: Type) (sOut :: Shape ) (b :: Bool) :: Type where
    MaybeValidINetwork net sOut 'False =
        TypeError ( Text "The network "
               :<>: ShowType net
               :<>: Text " is not compatible with the output "
               :<>: ShowType sOut
        )
    MaybeValidINetwork net sOut 'True = net

type family MkValidINetwork (layers :: [Type]) (sIn :: Shape) (sOut :: Shape) :: Type where
    MkINetwork ls sIn sOut =
        MaybeValidINetwork (INetwork ls (ComposeOut ls sIn)) sOut (ValidateOutput ls sIn sOut)

class ValidNetwork (xs :: [Type]) (ss :: [Shape]) where
    validNetwork :: INetwork xs ss

    {-# MINIMAL validNetwork #-}

instance (SingI i) => ValidNetwork '[] '[i] where
    validNetwork = INNil

instance ( SingI i
        , SingI o
        , Layer x
        , ValidNetwork xs (o ': rs)
        , (Out x i) ~ o
        ) => ValidNetwork (x ': xs) (i ': o ': rs) where
    validNetwork = layer :~> validNetwork

type MyNet = Network '[ Relu, Sigmoid, Flatten ]
type MyNet2 = INetwork '[ Relu, Sigmoid, Flatten ] (ComposeOut '[ Relu, Sigmoid, Flatten ] ('D2 28 28))
type MyNet3 = MkINetwork '[ Relu, Sigmoid, Flatten ] ('D2 28 28)
type MyNet4 = MkINetwork '[ Relu, Flatten ] ('D2 28 28)
type MyNet5 = MkValidINetwork '[ Relu, Flatten ] ('D2 28 28) ('D1 784)

myNet :: MyNet5
-- myNet :: INetwork '[ Relu, Flatten ] '[ 'D2 28 28, 'D2 28 28, 'D1 784]
myNet = validNetwork

-- type family CreateNetwork  (net :: Network layers)
--                            (input :: Shape) :: INetwork layers (ComposeOut input layers)
-- createNetwork :: Network layers -> (input :: Shape) -> INetwork layers (ComposeOut input layers)
-- createNetwork NNil s = INNil
--               -> (input :: Shape)
--               -> INetwork layers (ComposeOut input layers)
-- createNetwork NNil s = INNil





