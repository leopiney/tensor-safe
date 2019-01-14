{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
-- {-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE NoStarIsType              #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE Trustworthy               #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}


module TensorSafe.Tensor where

import           Data.Int          (Int16, Int64, Int8)
import           Data.Maybe        (fromJust)
import           Data.Proxy        (Proxy (..))
import qualified Data.Vector       as VN
import           Data.Vector.Sized (Vector (..), fromList, toList)
import           Data.Word         (Word8)
import           GHC.TypeLits      (KnownNat, Nat, natVal)
import           GHC.TypeLits


import           TensorSafe.Shape

data ValidType = DT_FLOAT | DT_INT | DT_BOOL deriving Show

class TensorType a where
    tensorType :: a -> ValidType

instance TensorType Float where
    tensorType _ = DT_FLOAT
instance TensorType Int where
    tensorType _ = DT_INT
instance TensorType Bool where
    tensorType _ = DT_BOOL


data Tensor v (s :: [Nat]) where
    Tensor :: (TensorType v) => v -> Shape s -> Tensor v s


instance Show (Tensor v s) where
    show (Tensor v s) = "Tensor [" ++ "] [" ++ show s ++ "]"


type family NatMult (a :: Nat) (b :: Nat) :: Nat where
    NatMult a 0 = 0
    NatMult a b = a + a + NatMult a (b - 1)


type family ShapeProduct (s :: [Nat]) :: Nat
type instance ShapeProduct '[] = 1
type instance ShapeProduct (m ': s) = NatMult m (ShapeProduct s)

-- constant :: (TensorType a, ShapeProduct s ~ n) => Vector n a -> Shape s -> Tensor a s

-- constant :: (TensorType a, ShapeProduct s ~ n) => a -> Shape s -> Tensor a s
-- constant t shp = Tensor t (toUnsafe shp)
