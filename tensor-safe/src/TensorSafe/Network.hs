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

import           Data.Kind         (Type)
import           Data.Singletons
-- import           Data.Singletons.Prelude

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

instance ( SingI i
         , SingI o
         , Layer x i o
         , ValidNetwork xs (o ': rs)
         ) => ValidNetwork (x ': xs) (i ': o ': rs) where
  validNetwork = layer :~~ validNetwork

-- asdf :: Type -> String
-- asdf _ = "asdf"
-- compile :: forall layers shapes. Network layers shapes -> S (Head shapes) -> String
-- compile (l :~~ n) !x = asdf (layer l)
  -- comp
  --   where
  -- comp :: forall xs ss. (Last ss ~ Last shapes)
  --      => Network xs ss -> S (Head ss) -> String
  -- comp NNil !x = "Nil"

-- compile (x :~~ n) =
--   comp
--     where
--   comp ::
-- runNetwork :: forall layers shapes.
--               Network layers shapes
--            -> S (Head shapes)
--            -> (Tapes layers shapes, S (Last shapes))
-- runNetwork =
--   go
--     where
--   go  :: forall js ss. (Last js ~ Last shapes)
--       => Network ss js
--       -> S (Head js)
--       -> (Tapes ss js, S (Last js))
--   go (layer :~> n) !x =
--     let (tape, forward) = runForwards layer x
--         (tapes, answer) = go n forward
--     in  (tape :\> tapes, answer)

--   go NNil !x
--       = (TNil, x)
