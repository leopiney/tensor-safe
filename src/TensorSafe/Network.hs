{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
module TensorSafe.Network where

import           Data.Kind               (Type)
import           Data.Singletons
import           Data.Text.Lazy          (Text, unpack)
import           Formatting
import           GHC.TypeLits
import           GHC.TypeLits.Extra      (Div)

import           TensorSafe.Compile.Expr
import           TensorSafe.Core
import           TensorSafe.Layer        (Layer, compile, compileCNet, layer)
import           TensorSafe.Layers
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

instance ValidNetwork ls ss => Layer (INetwork ls ss) where
    layer = mkINetwork
    compile n = compileNetwork' n
    compileCNet n i = toCNetwork' n True i

--
--
--

-- | Returns the result of applying all the layers transformation to a specific shape.
--   Given a list of layers, this returns the expected output for the computation of each layer
--   starting with the first layer transforming the Shape `s`.
--   For example, if the initial Shape is [28, 28] and the layers are [Relu, Flatten], the result
--   will be [784].
type family ComputeOut (layers :: [Type]) (s :: Shape) :: Shape where
    ComputeOut '[] s      = s
    ComputeOut (l : ls) s = ComputeOut ls (Out l s)

-- | Returns a list of shapes describing all the transformations applied to a specific shape.
--   Given a list of layers return a type with all the Shapes from the initial Shape until the
--   last one. In theory, the last Shape should be the same than the ComputeOut function applied
--   to this same parameters.
type family ComposeOut' (layers :: [Type]) (s :: Shape) :: [Shape] where
    ComposeOut' '[] s      = '[]
    ComposeOut' (l : ls) s = ((Out l s) ': (ComposeOut' ls (Out l s)))

-- | Same than ComposeOut' but the Shape list includes the initial Shape
type family ComposeOut (layers :: [Type]) (s :: Shape) :: [Shape] where
    ComposeOut '[] s = '[]
    ComposeOut ls s  = s ': (ComposeOut' ls s)

-- | Compares the layers shape computation and the expected output
type family ValidateOutput (layers :: [Type]) (sIn :: Shape) (sOut :: Shape) :: Bool where
    ValidateOutput ls sIn sOut = ShapeEquals' (ComputeOut ls sIn) sOut

--
--
--

-- | TODO
type family MkINetworkUnsafe (layers :: [Type]) (s :: Shape) :: Type where
    MkINetworkUnsafe ls s = INetwork ls (ComposeOut ls s)

-- | TODO
type family MaybeType (t :: Type) (b :: Bool) :: Type where
    MaybeType t 'False = Type -- HACK: ValidateOutput should raise an exception on this case
    MaybeType t 'True  = t

-- | TODO
type family MkINetwork (layers :: [Type]) (sIn :: Shape) (sOut :: Shape) :: Type where
    MkINetworkUnsafe ls sIn sOut =
        MaybeType (INetwork ls (ComposeOut ls sIn)) (ValidateOutput ls sIn sOut)

--
--
--

-- | Defines the expected output of a layer
--   This type function should be instanciated for each of the Layers defined.
type family Out (l :: Type) (s :: Shape) :: Shape where
    --
    --
    --
    Out (INetwork ls (s : ss)) s = ComputeOut ls s

    --
    --
    --
    Out (Conv2D 1 1 k k' s s') ('D2 inputRows inputColumns) =
        ('D2 (1 + (Div (inputRows - k) s))
                (1 + (Div (inputColumns - k') s'))
        )

    Out (Conv2D 1 filters k k' s s') ('D2 inputRows inputColumns) =
        ('D3 (1 + (Div (inputRows - k) s))
                (1 + (Div (inputColumns - k') s'))
                filters
        )

    Out (Conv2D channels 1 k k' s s') ('D3 inputRows inputColumns channels) =
        ('D2 (1 + (Div (inputRows - k) s))
                (1 + (Div (inputColumns - k') s'))
        )

    Out (Conv2D channels filters k k' s s') ('D3 inputRows inputColumns channels) =
        ('D3 (1 + (Div (inputRows - k) s))
                (1 + (Div (inputColumns - k') s'))
                filters
        )

    --
    --
    --
    Out (Dense i o) ('D1 i) = 'D1 o

    --
    --
    --
    Out (Dropout rate seed) s = s

    --
    --
    --
    Out Flatten ('D1 x)     = 'D1 x
    Out Flatten ('D2 x y)   = 'D1 (NatMult x y)
    Out Flatten ('D3 x y z) = 'D1 (NatMult3 x y z)

    --
    --
    --
    Out (MaxPooling k k' s s') ('D2 inputRows inputColumns) =
        ('D2 (1 + (Div (inputRows - k) s))
                (1 + (Div (inputColumns - k') s'))
        )

    Out (MaxPooling k k' s s') ('D3 inputRows inputColumns channels) =
        ('D3 (1 + (Div (inputRows - k) s))
                (1 + (Div (inputColumns - k') s'))
                channels
        )

    --
    --
    --
    Out Relu s           = s

    --
    --
    --
    Out Sigmoid s           = s

    --
    -- Edge case or not defined raise an error
    --
    Out l sOut =
        TypeError ( 'Text "Couldn't apply the Layer \""
                ':<>: 'ShowType l
                ':<>: 'Text "\" with the output Shape \""
                ':<>: 'ShowType sOut
                ':<>: 'Text "\"")

--
--
--

-- | Instanciates a Network after defining a type definition, using MkINetworkUnsafe for example.
--   After defining a variable with INetwork type, you can instanciate that variable like this:
--
--   myNet :: MNIST
--   myNet = mkINetwork
class ValidNetwork (xs :: [Type]) (ss :: [Shape]) where
    mkINetwork :: INetwork xs ss

    {-# MINIMAL mkINetwork #-}

instance (SingI i) => ValidNetwork '[] '[i] where
    mkINetwork = INNil

instance ( SingI i
         , SingI o
         , Layer x
         , ValidNetwork xs (o ': rs)
         , (Out x i) ~ o -- IMPORTANT: validation that the output and the computation of the layer
                         -- will match. Without this constraint we could be able to create an
                         -- instance of ValidNetwork that doesn't satisfies the type constraints
                         -- of MkINetwork for example.
      ) => ValidNetwork (x ': xs) (i ': o ': rs) where
    mkINetwork = layer :~> mkINetwork


--
--Compilation preliminar stuff
--
compileNetwork ::
    forall i x xs ss. ( SingI i
                      , Layer x
                      , ValidNetwork (x ': xs) (i ': ss)) => INetwork (x ': xs) (i ': ss) -> Text
compileNetwork n =
    case (sing :: Sing i) of
        D1Sing a ->     compileNetworkInput n [ fromInteger (natVal a)]
        D2Sing a b ->   compileNetworkInput n [ fromInteger (natVal a)
                                              , fromInteger (natVal b)]
        D3Sing a b c -> compileNetworkInput n [ fromInteger (natVal a)
                                              , fromInteger (natVal b)
                                              , fromInteger (natVal c)]

-- | Compiles a network recursivelly
compileNetwork' :: INetwork xs ss -> String -> Text
compileNetwork' INNil _    = ""
compileNetwork' (l :~> n) inputShape =
    let compilatedLayer = unpack (compile l inputShape)
        compilatedNetwork = unpack (compileNetwork' n "undefined")
    in format ("" % string % "\n  " % string) compilatedLayer compilatedNetwork


compileNetworkInput :: INetwork xs ss -> [Integer] -> Text
compileNetworkInput n inputShape =
    let compilatedNetwork = unpack (compileNetwork' n (show inputShape))
    in
        format ("import * as tf from '@tensorflow/tfjs';\n\
            \\n\
            \\n\
            \ function createModel() {\n\
            \  const model = tf.sequential();\n\
            \\n\
            \  " % string % "\n\
            \  return model\n\
            \}\n") compilatedNetwork


--
-- Compilation: More advanced stuff
--

toCNetwork ::
    forall i x xs ss. ( SingI i
                      , Layer x
                      , ValidNetwork (x ': xs) (i ': ss)) => INetwork (x ': xs) (i ': ss) -> CNetwork
toCNetwork n =
    case (sing :: Sing i) of
        D1Sing a     -> CNSequence (toCNetwork' n False (Just $ show [ natVal a]))

        D2Sing a b   -> CNSequence (toCNetwork' n False (Just $ show [ natVal a
                                                                     , natVal b]))

        D3Sing a b c -> CNSequence (toCNetwork' n False (Just $ show [ natVal a
                                                                     , natVal b
                                                                     , natVal c]))

toCNetwork' :: INetwork xs ss -> Bool -> Maybe String -> CNetwork
toCNetwork' INNil nested _ =
    if nested
        then CNNil
        else CNReturn
toCNetwork' (l :~> n) nested inputShape =
    let compilatedLayer = compileCNet l inputShape
        compilatedNetwork = toCNetwork' n nested Nothing
    in CNCons compilatedLayer compilatedNetwork
