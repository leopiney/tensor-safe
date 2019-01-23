{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module TensorSafe.Tensor where

import           Data.Proxy       (Proxy (..))
import           GHC.TypeLits

import           TensorSafe.Shape

--
-- Define valid tensor types
--
data TensorType v where
    DT_FLOAT :: TensorType Float
    DT_INT :: TensorType Int
    DT_BOOL :: TensorType Bool

instance Show (TensorType v) where
    show DT_BOOL  = "BOOL"
    show DT_FLOAT = "FLOAT"
    show DT_INT   = "INT"

class ValidTensorType a

instance ValidTensorType Float
instance ValidTensorType Int
instance ValidTensorType Bool

--
-- Define Tensor structure
--
data Tensor t (s :: [Nat]) where
    Tensor :: (ValidTensorType t) => TensorType t -> Shape s -> Tensor t s

instance Show (Tensor t s) where
    show (Tensor t s) = "Tensor <" ++ show t ++ "> [" ++ show s ++ "]"


--
-- Natural number operations helpers
--
type family NatMult (a :: Nat) (b :: Nat) :: Nat where
    NatMult a 0 = 0
    NatMult a b = a + a + NatMult a (b - 1)


type family ShapeProduct (s :: [Nat]) :: Nat
type instance ShapeProduct '[] = 1
type instance ShapeProduct (m ': s) = NatMult m (ShapeProduct s)

constant :: (ValidTensorType t) => TensorType t -> Shape s -> Tensor t s
constant t s = Tensor t s

add :: (ValidTensorType t) => Tensor t s -> Tensor t s -> Tensor t s
add t1 _ = t1

matMult :: (ValidTensorType t) => Tensor t '[i, n] -> Tensor t '[n, o] -> Tensor t '[i, o]
matMult
    (Tensor t ((i :: Proxy m) :-- _))
    (Tensor _ (_ :-- (o :: Proxy m2) :-- _)) = Tensor t (i :-- o :-- Nil)
