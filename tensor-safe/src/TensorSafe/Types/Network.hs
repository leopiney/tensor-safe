{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module TensorSafe.Types.Network where

import           Data.Kind        (Type)
import           Data.Singletons
import           Data.Typeable    (typeOf)

import           TensorSafe.Core
import           TensorSafe.Shape


data Flatten = Flatten deriving Show
data Relu = Relu deriving Show
data Sigmoid = Sigmoid deriving Show


data Network :: [Type] -> Type where
    NNil  :: Network '[]

    (:~~) :: !x
          -> !(Network xs)
          -> Network (x ': xs)
infixr 5 :~~


data INetwork :: [Type] -> [Shape] -> Type where
    INNil  :: SingI i
            => INetwork '[] '[i]

    (:~>) :: (SingI i, SingI h)
            => !x
            -> !(INetwork xs (h ': hs))
            -> INetwork (x ': xs) (i ': h ': hs)
infixr 5 :~>



type MyNet = Network '[ Relu, Sigmoid, Flatten ]

type family Out (s :: Shape) (l :: Type) :: Shape where
    Out s Relu    = s
    Out s Sigmoid = s
    Out ('D1 x) Flatten = 'D1 x
    Out ('D2 x y) Flatten = 'D1 (NatMult x y)
    Out ('D3 x y z) Flatten = 'D1 (NatMult3 x y z)

type family ComputeOut (s :: Shape) (layers :: [Type]) :: Shape where
    ComputeOut s '[]      = s
    ComputeOut s (l : ls) = ComputeOut (Out s l) ls

type family ComposeOut (s :: Shape) (layers :: [Type]) :: [Shape] where
    ComputeOut s '[]      = '[s]
    ComputeOut s (l : ls) = (s ': (ComposeOut s ls))


type MyNet2Layers = '[ Relu, Sigmoid, Flatten ]
type MyNet2 = INetwork MyNet2Layers (ComposeOut ('D2 28 28) MyNet2Layers)


-- type family CreateNetwork  (net :: Network layers)
--                            (input :: Shape) :: INetwork layers (ComposeOut input layers)
-- createNetwork :: Network layers -> (input :: Shape) -> INetwork layers (ComposeOut input layers)
-- createNetwork NNil s = INNil
--               -> (input :: Shape)
--               -> INetwork layers (ComposeOut input layers)
-- createNetwork NNil s = INNil





