{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module TensorSafe.Generic.Shape (
    Shape(..),
    UnsafeShape(..),
    buildShape,
    fromUnsafe,
    toUnsafe,
) where

import           Data.Maybe   (fromJust)
import           Data.Proxy   (Proxy (..))
import           GHC.TypeLits (KnownNat, Nat, natVal)

newtype UnsafeShape = UnsafeShape [Int] deriving (Eq, Show)

--
-- Define the safe Shape data kind
--
infixr 5 :~>

data Shape :: [Nat] -> * where
    SNil :: Shape '[]
    (:~>) :: KnownNat m => Proxy m -> Shape s -> Shape (m ': s)


showShape :: (Shape s) -> String
showShape SNil                       = ""
showShape ((pm :: Proxy m) :~> SNil) = show (natVal pm)
showShape ((pm :: Proxy m) :~> s)    = show (natVal pm) ++ "," ++ showShape s

instance Show (Shape s) where
    show s = "[" ++ (showShape s) ++ "]"

--
-- Create a MkShape class that allows to convert a regular list
-- into a safe Shape
--
class MkShape (s :: [Nat]) where
  mkShape :: Shape s

instance MkShape '[] where
  mkShape = SNil

instance (MkShape s, KnownNat m) => MkShape (m ': s) where
  mkShape = Proxy :~> mkShape

toUnsafe :: Shape s -> UnsafeShape
toUnsafe SNil = UnsafeShape []
toUnsafe ((pm :: Proxy m) :~> s) = UnsafeShape (fromInteger (natVal pm) : s')
    where
        (UnsafeShape s') = toUnsafe s

fromUnsafe :: forall s. MkShape s => UnsafeShape -> Maybe (Shape s)
fromUnsafe shape = if toUnsafe myShape == shape
    then Just myShape
    else Nothing
    where
        myShape = mkShape :: Shape s

buildShape :: forall s. MkShape s => [Int] -> Shape s
buildShape = fromJust . fromUnsafe . UnsafeShape
