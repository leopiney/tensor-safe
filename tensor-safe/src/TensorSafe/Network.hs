{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module TensorSafe.Network where

import           Data.Kind         (Type)
import           Data.Singletons

import           TensorSafe.Layers
import           TensorSafe.Shape

-- | TODO
data Network :: [Type] -> [Shape] -> Type where
    NNil  :: SingI i
            => Network '[] '[i]

    (:~~) :: (SingI i, SingI h, Layer x i h)
            => !x
            -> !(Network xs (h ': hs))
            -> Network (x ': xs) (i ': h ': hs)
infixr 5 :~~


instance Show (Network '[] '[i]) where
  show NNil = "NNil"

instance (Show x, Show (Network xs rs)) => Show (Network (x ': xs) (i ': rs)) where
  show (x :~~ xs) = show x ++ "\n :~~ " ++ show xs

-- | TODO
class ValidNetwork (xs :: [Type]) (ss :: [Shape]) where
  validNetwork :: Network xs ss

  {-# MINIMAL validNetwork #-}

instance (SingI i) => ValidNetwork '[] '[i] where
  validNetwork = NNil

instance (SingI i, SingI o, Layer x i o, ValidNetwork xs (o ': rs)) => ValidNetwork (x ': xs) (i ': o ': rs) where
  validNetwork = layer :~~ validNetwork
