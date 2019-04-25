{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-| This module is the core of TensorSafe. It defines all Network data structures
-- and types functions that respresent Layers modifications of shapes, as well as
-- all needed information for compiling the Network structures to CNetworks for later code
-- generation.
-}
-- module TensorSafe.Network (
--     Network (..),
--     INetwork (..),
--     MkINetwork,
--     ValidNetwork,
--     mkINetwork,
--     toCNetwork
-- ) where
module TensorSafe.Network where

import           Data.Kind               (Type)
import           Data.Singletons
import           GHC.TypeLits            as N
import           GHC.TypeLits.Extra      (Div)

import           TensorSafe.Compile.Expr
import           TensorSafe.Layer        (Layer, compile, layer)
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
-- transformation along the network. It's an Instance of a Network: given a Network and a initial
-- Shape, this type structure can be generated automatically using the type functions defined in
-- this module, like `Out` and `MkINetwork`.
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

-- | This instance of INetwork as a Layer makes possible nesting INetworks
instance ValidNetwork ls ss => Layer (INetwork ls ss) where
    layer = mkINetwork
    compile n i = toCNetwork' n True i

--
-- COMPUTING RESULTING SHAPES FROM A LIST OF LAYERS.
--

-- | Returns the result of applying all the layers transformation to a specific shape.
-- Given a list of layers, this returns the expected output for the computation of each layer
-- starting with the first layer transforming the `Shape` s.
-- For example, if the initial Shape is [28, 28] and the layers are [Relu, Flatten], the result
-- will be [784].
type family ComputeOut (layers :: [Type]) (s :: Shape) :: Shape where
    ComputeOut '[] s      = s
    ComputeOut (l : ls) s = ComputeOut ls (Out l s)

-- | Returns a list of shapes describing ALL the transformations applied to a specific shape.
-- Given a list of layers return a type with all the Shapes from the initial Shape until the
-- last one. In theory, the last Shape should be the same than the ComputeOut function applied
-- to this same parameters.
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
-- CREATE INETWORK TYPE INSTANCES FROM LIST OF LAYERS AND INTIAL AND ENDING SHAPES
--

-- | Creates an INetwork type, and by "unconstrained" I mean that I don't check for an
--   expected output
type family MkINetworkUnconstrained (layers :: [Type]) (s :: Shape) :: Type where
    MkINetworkUnconstrained ls s = INetwork ls (ComposeOut ls s)

-- | If the second type argument is 'True, then it returns the type t, otherwise it returns
--   a default type. Note that for this example, ValidateOutput would raise an exception
--   if the expected output and the actual one do not match.
type family MaybeType (t :: Type) (b :: Bool) :: Type where
    MaybeType t 'False = Type -- HACK: ValidateOutput should raise an exception on this case
    MaybeType t 'True  = t

-- | Creates an INetwork type validating the the expected output and the computed one match.
type family MkINetwork (layers :: [Type]) (sIn :: Shape) (sOut :: Shape) :: Type where
    MkINetworkUnconstrained ls sIn sOut =
        MaybeType (INetwork ls (ComposeOut ls sIn)) (ValidateOutput ls sIn sOut)

--
-- MAPPING TRANSFORMATIONS OF LAYERS AND SHAPES
--

type family MaybeShape (s :: Shape) (b :: Bool) :: Shape where
    MaybeType s 'False = 'D1 0 -- HACK: ShapeEquals' should raise an exception on this case
    MaybeType s 'True  = s

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
    Out (Add ls1 ls2) sIn =
        MaybeShape
            (ComputeOut ls1 sIn)
            (ShapeEquals' (ComputeOut ls1 sIn) (ComputeOut ls2 sIn))  -- Validation that computes the same
    -- Out (Add (INetwork ls (s : ss))) s = ComputeOut ls s

    --
    --
    --
    Out (BatchNormalization _ _ _) s = s

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
    Out Flatten ('D2 x y)   = 'D1 (x N.* y)
    Out Flatten ('D3 x y z) = 'D1 (x N.* y N.* z)

    --
    --
    --
    Out GlobalAvgPooling2D ('D3 _ _ z) = 'D1 z

    --
    --
    --
    Out Input s = s

    --
    --
    --
    Out (LSTM units 'False) _           = 'D1 units
    Out (LSTM units 'True)  ('D2 x _)   = 'D2 x units
    Out (LSTM units 'True)  ('D3 x _ _) = 'D2 x units

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
    Out Relu s = s

    --
    --
    --
    Out Sigmoid s = s

    --
    --
    --
    Out (ZeroPadding2D padding_rows padding_cols) ('D2 inputRows inputColumns) =
        ('D2 (inputRows + (2 N.* padding_rows)) (inputColumns + (2 N.* padding_cols)))

    Out (ZeroPadding2D padding_rows padding_cols) ('D3 inputRows inputColumns channels) =
        ('D3 (inputRows + (2 N.* padding_rows)) (inputColumns + (2 N.* padding_cols)) channels)

    --
    -- Edge case or not defined raise an error
    --
    Out l sIn =
        TypeError ( 'Text "Couldn't apply the Layer \""
                ':<>: 'ShowType l
                ':<>: 'Text "\" with the input Shape \""
                ':<>: 'ShowType sIn
                ':<>: 'Text "\"")

--
-- INETWORK VALIDATION
--

-- | Instanciates a Network after defining a type definition,
--   using MkINetworkUnconstrained or MkINetwork, for example.
--   After defining a variable with INetwork type, you can instanciate that variable like this:
--   ```
--       myNet :: MNIST
--       myNet = mkINetwork
--   ```
class ValidNetwork (xs :: [Type]) (ss :: [Shape]) where

    -- | Makes a valid instance of INetwork
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
-- INETWORK MAPPING TO CNETWORK
--

-- | Compilation: Gets the initial shape using Singleton instances. Since this is the function we
--   run for transforming an INetwork to CNetwork, the nested argument of `toCNetwork'` is set
--   to False.
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
-- | Helper function for `toCNetwork`
toCNetwork' :: INetwork xs ss -> Bool -> Maybe String -> CNetwork
toCNetwork' INNil nested _ =
    if nested
        then CNNil
        else CNReturn
toCNetwork' (l :~> n) nested inputShape =
    let compilatedLayer = compile l inputShape
        compilatedNetwork = toCNetwork' n nested Nothing
    in CNCons compilatedLayer compilatedNetwork
