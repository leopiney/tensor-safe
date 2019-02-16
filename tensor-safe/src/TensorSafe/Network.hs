{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module TensorSafe.Network where


import           Data.Singletons
import           TensorSafe.Shape

class Layer x (i :: Shape s) (o :: Shape s1)


data Network :: [*] -> [Shape s] -> * where
    NNil  :: SingI i
            => Network '[] '[i]

    (:~~) :: (SingI i, SingI h, Layer x i h)
            => !x
            -> !(Network xs (h ': hs))
            -> Network (x ': xs) (i ': h ': hs)
infixr 5 :~~

myShape :: Shape [28, 28, 1]
myShape = buildShape [1, 3]

type MyNet = Network '[ ] '[ 'SNil ]
