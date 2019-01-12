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

data ValidType = DT_FLOAT | DT_INT | DT_BOOL

class TensorType a where
    tensorType :: a -> ValidType

instance TensorType Float where
    tensorType _ = DT_FLOAT
instance TensorType Int where
    tensorType _ = DT_INT
instance TensorType Bool where
    tensorType _ = DT_BOOL

data Tensor v (s :: [Nat]) where
    Tensor :: (TensorType v) => v -> Tensor v s

-- type family NatMult (p :: (Nat, Nat, Nat)) :: Nat
-- type instance NatMult (m n 0) = 0
-- type instance NatMult m = (fst3 m) + (snd3 m) + NatMult ((fst3 m), (snd3 m), (thd3 m) - 1)

-- type family NatMult (p :: (Nat, Nat)) :: Nat
-- type instance NatMult (p m 0) = 0
-- type instance NatMult (p m n) = m + NatMult (m, n)
-- type instance NatMult (p m n o) = m + m + NatMult (m, m, ( - 1))

-- type family ShapeProduct (s :: [Nat]) :: Nat
-- type instance ShapeProduct '[] = 1
-- type instance ShapeProduct (0 ': s) = ShapeProduct s
-- type instance ShapeProduct (m ': s) = ShapeProduct ((m - 1) ': s)

type family NatDouble (s :: (Nat, Nat)) :

type family ShapeProduct (s :: [Nat]) :: Nat
type instance ShapeProduct '[] = 1
type instance ShapeProduct (m ': s) = ShapeProduct s + m

-- constant :: (TensorType a, ShapeProduct s ~ n) => Vector n a -> Shape s -> Tensor a s
