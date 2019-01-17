{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


module TensorSafe.Tensor where

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
data Tensor v (s :: [Nat]) where
    Tensor :: (ValidTensorType v) => TensorType v -> Shape s -> Tensor v s

instance Show (Tensor v s) where
    show (Tensor v s) = "Tensor [" ++ show v ++ "] [" ++ show s ++ "]"


--
-- Natural number operations helpers
--
type family NatMult (a :: Nat) (b :: Nat) :: Nat where
    NatMult a 0 = 0
    NatMult a b = a + a + NatMult a (b - 1)


type family ShapeProduct (s :: [Nat]) :: Nat
type instance ShapeProduct '[] = 1
type instance ShapeProduct (m ': s) = NatMult m (ShapeProduct s)

-- constant :: (TensorType a, ShapeProduct s ~ n) => Vector n a -> Shape s -> Tensor a s

-- constant :: (TensorType a, ShapeProduct s ~ n) => a -> Shape s -> Tensor a s
-- constant t shp = Tensor t (toUnsafe shp)
