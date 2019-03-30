{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module TensorSafe.Layers.Conv2D where

import           Data.Kind               (Type)
import           Data.Map
import           Data.Proxy
import           Data.Typeable           (typeOf)
import           GHC.TypeLits

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer


-- | TODO
data Conv2D :: Nat -> Nat -> Nat -> Nat -> Nat -> Nat -> Type where
    Conv2D :: Conv2D channels filters kernelRows kernelColumns strideRows strideColumns

instance ( KnownNat c
         , KnownNat f
         , KnownNat k
         , KnownNat k'
         , KnownNat s
         , KnownNat s'
         ) => Show (Conv2D c f k k' s s') where
        show = show . typeOf


instance ( KnownNat channels
         , KnownNat filters
         , KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         ) => Layer (Conv2D channels filters kernelRows kernelColumns strideRows strideColumns) where
    layer = Conv2D
    compile _ inputShape =
        let filters = natVal (Proxy :: Proxy filters)
            kernelRows = natVal (Proxy :: Proxy kernelRows)
            kernelColumns = natVal (Proxy :: Proxy kernelColumns)
            strideRows = natVal (Proxy :: Proxy strideRows)
            strideColumns = natVal (Proxy :: Proxy strideColumns)

            initialParams = case inputShape of
                Just shape -> fromList [("inputShape", shape)]
                Nothing    -> empty
            params = union initialParams (fromList [
                    ("kernelSize", show [kernelRows, kernelColumns]),
                    ("filters", show filters),
                    ("strides", show [strideRows, strideColumns])
                ])
        in
            CNLayer "conv2d" params
