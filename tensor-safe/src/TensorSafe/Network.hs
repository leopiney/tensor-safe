{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module TensorSafe.Network where


import           Data.Singletons
import           GHC.TypeLits
-- import           TensorSafe.Shape

data Shape = D1 Nat | D2 Nat Nat | D3 Nat Nat Nat

class Layer x (i :: Shape) (o :: Shape)

data Network :: [*] -> [Shape] -> * where
    NNil  :: SingI i
            => Network '[] '[i]

    (:~~) :: (SingI i, SingI h, Layer x i h)
            => !x
            -> !(Network xs (h ': hs))
            -> Network (x ': xs) (i ': h ': hs)
infixr 5 :~~

data R (n :: Nat) where
  R :: (KnownNat n) => R n

data L (m :: Nat) (n :: Nat) where
  L :: (KnownNat m, KnownNat n) => L m n

-- | A basic fully connected (or inner product) neural network layer.
data FullyConnected (i :: Nat) (o :: Nat) = FullyConnected
                        !(FullyConnected' i o)   -- Neuron weights
                        !(FullyConnected' i o)   -- Neuron momentum

data FullyConnected' (i :: Nat) (o :: Nat) = FullyConnected'
                         !(R o)   -- Bias
                         !(L o i) -- Activations

instance Show (FullyConnected i o) where
  show FullyConnected {} = "FullyConnected"

data Logit = Logit
  deriving Show
