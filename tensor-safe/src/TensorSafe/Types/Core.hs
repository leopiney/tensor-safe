{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module TensorSafe.Types.Core where

import           Data.Kind        (Type)
import           GHC.TypeLits
import           TensorSafe.Shape

-- | Compares two Shapes at kinds level and returns a Bool kind
type family ShapeEquals (sIn :: Shape) (sOut :: Shape) :: Bool where
    ShapeEquals s s = 'True
    ShapeEquals _ _ = 'False

type family ShapeEquals' (sIn :: Shape) (sOut :: Shape) :: Bool where
    ShapeEquals' s s = 'True
    ShapeEquals' s1 s2 =
        TypeError ( 'Text "Couldn't match the Shape "
              ':<>: 'ShowType s1
              ':<>: 'Text " with the Shape "
              ':<>: 'ShowType s2)

-- | Compares two types in kinds level
type family TypeEquals (s1 :: Type) (s2 :: Type) :: Bool where
    TypeEquals s s = 'True
    TypeEquals _ _ = 'False

-- | Compares two types in kinds level and raises error if they don't match
type family TypeEquals' s1 s2 :: Type where
    TypeEquals' s s = s
    TypeEquals' s1 s2 =
        TypeError ( 'Text "Couldn't match the type "
              ':<>: 'ShowType s1
              ':<>: 'Text " with type "
              ':<>: 'ShowType s2)
