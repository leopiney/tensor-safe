{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module TensorSafe.Network where

import           Data.Kind           (Type)
import           Data.Singletons

import           TensorSafe.Layer
import           TensorSafe.LayerOut (Out)
import           TensorSafe.Shape

-- | A network that defines a specific sequence of layers
data Network :: [Type] -> Type where
  NNil  :: Network '[]

  (:~~) :: Layer x
        => !x
        -> !(Network xs)
        -> Network (x ': xs)
infixr 5 :~~

instance Show (Network '[]) where
  show NNil = "NNil"

instance (Show x, Show (Network xs)) => Show (Network (x ': xs)) where
  show (x :~~ xs) = show x ++ "\n :~~ " ++ show xs

-- | A network that defines a specific sequence of layers with the corresponding shape
--   transformation along the network.
data INetwork :: [Type] -> [Shape] -> Type where
  INNil  :: SingI i
          => INetwork '[] '[i]

  (:~>) :: (SingI i, SingI h, Layer x)
        => !x
        -> !(INetwork xs (h ': hs))
        -> INetwork (x ': xs) (i ': h ': hs)
infixr 5 :~>

instance Show (INetwork '[] '[i]) where
  show INNil = "NNil"

instance (Show x, Show (INetwork xs rs)) => Show (INetwork (x ': xs) (i ': rs)) where
  show (x :~> xs) = show x ++ "\n :~> " ++ show xs

-- | Instanciates a Network after defining a type definition, using MkINetwork for example.
--   After defining a variable with INetwork type, you can instanciate that variable like this:
--
--   myNet :: MNIST
--   myNet = validNetwork
class ValidNetwork (xs :: [Type]) (ss :: [Shape]) where
  validNetwork :: INetwork xs ss

  {-# MINIMAL validNetwork #-}

instance (SingI i) => ValidNetwork '[] '[i] where
  validNetwork = INNil

instance ( SingI i
         , SingI o
         , Layer x
         , ValidNetwork xs (o ': rs)
         , (Out x i) ~ o -- IMPORTANT: validation that the output and the computation of the layer
                         -- will match. Without this constraint we could be able to create an
                         -- instance of ValidNetwork that doesn't satisfies the type constraints
                         -- of MkValidINetwork for example.
         ) => ValidNetwork (x ': xs) (i ': o ': rs) where
    validNetwork = layer :~> validNetwork
