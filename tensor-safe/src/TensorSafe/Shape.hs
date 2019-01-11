{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}
module TensorSafe.Shape () where

import           Data.Proxy        (Proxy (..))
import           Data.Vector.Sized (Vector, fromList)
import           GHC.TypeLits      (KnownNat, Nat)

newtype UnsafeShape = UnsafeShape [Int]
instance Show UnsafeShape

data Shape (s :: [Nat]) where
    Nil :: Shape '[]
    (:--) :: KnownNat m => Proxy m -> Shape s -> Shape (m ': s)
