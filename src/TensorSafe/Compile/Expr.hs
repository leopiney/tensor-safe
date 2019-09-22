{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-| This module describes the expression structure of a INetwork instance.
-- The INetwork can be structured into a Data structure called CNetwork, with which later
-- to compilation external languages can be done.
-}
module TensorSafe.Compile.Expr (
    DLayer (..),
    CNetwork (..),
    JavaScript (..),
    Python (..),
    Generator,
    generate,
    generateFile,
    toCNetwork
) where

import           Data.Map
import           Data.Singletons
import           Data.Text.Lazy as T
import           Formatting
import           Text.Casing    (camel, quietSnake)
import           GHC.TypeLits


import           TensorSafe.Layer
import           TensorSafe.Network
import           TensorSafe.Shape


-- | This instance of INetwork as a Layer makes possible nesting INetworks
instance ValidNetwork ls ss => Layer (INetwork ls ss) where
    layer = mkINetwork
    compile n i = toCNetwork' n True i

--
-- INETWORK MAPPING TO CNETWORK
--

-- | Compilation: Gets the initial shape using Singleton instances. Since this is the function we
--   run for transforming an INetwork to CNetwork, the nested argument of `toCNetwork'` is set
--   to False.
toCNetwork ::
    forall i x xs ss. ( SingI i
                      , Layer x
                      , ValidNetwork (x ': xs) (i ': ss)
                      ) => INetwork (x ': xs) (i ': ss) -> CNetwork
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



-- | Support for JavaScript compilation
data JavaScript = JavaScript deriving Show

-- | Support for Python compilation
data Python = Python deriving Show

-- | Defines how are the layers going to be translated to the domain language
-- This translates DLayer to String for each supported language
class LayerGenerator l where
    generateName :: l -> DLayer -> String

instance LayerGenerator JavaScript where
    generateName _ DActivation         = "activation"
    generateName _ DAdd                = "addStrict"
    generateName _ DBatchNormalization = "batchNormalization"
    generateName _ DConcatenate        = "concatenate"
    generateName _ DConv2D             = "conv2d"
    generateName _ DDense              = "dense"
    generateName _ DDropout            = "dropout"
    generateName _ DFlatten            = "flatten"
    generateName _ DGlobalAvgPooling2D = "globalAvgeragePooling2D"
    generateName _ DInput              = "input"
    generateName _ DLSTM               = "lstm"
    generateName _ DMaxPooling         = "maxPooling2d"
    generateName _ DRelu               = "reLU"
    generateName _ DSoftmax            = "softmax"
    generateName _ DUpSampling         = "upSampling2D"
    generateName _ DZeroPadding2D      = "zeroPadding2D"

instance LayerGenerator Python where
    generateName _ DActivation         = "Activation"
    generateName _ DAdd                = "add"
    generateName _ DBatchNormalization = "BatchNormalization"
    generateName _ DConcatenate        = "Concatenate"
    generateName _ DConv2D             = "Conv2D"
    generateName _ DDense              = "Dense"
    generateName _ DDropout            = "Dropout"
    generateName _ DFlatten            = "Flatten"
    generateName _ DGlobalAvgPooling2D = "GlobalAvgeragePooling2D"
    generateName _ DInput              = "Input"
    generateName _ DLSTM               = "LSTM"
    generateName _ DMaxPooling         = "MaxPool2D"
    generateName _ DRelu               = "ReLu"
    generateName _ DSoftmax            = "Softmax"
    generateName _ DUpSampling         = "UpSampling2D"
    generateName _ DZeroPadding2D      = "ZeroPadding2D"

-- | Class that defines which languages are supported for CNetworks generation to text
class Generator l where

    -- | Adds supports for a language. Generates a CNetwork to Text
    generate :: forall i x xs ss. ( SingI i
                                   , Layer x
                                   , ValidNetwork (x ': xs) (i ': ss)
                                   ) => l -> INetwork (x ': xs) (i ': ss) -> Text

    -- | Similar to 'generate', but also adds necessary header and module lines of text so as to
    -- have the CNetwork compiled at a separate file.
    generateFile :: forall i x xs ss. ( SingI i
                                      , Layer x
                                      , ValidNetwork (x ': xs) (i ': ss)
                                      ) => l -> INetwork (x ': xs) (i ': ss) -> Text

instance Generator JavaScript where
    generate l =
        T.intercalate "\n" . generateJS . toCNetwork
        where
            generateJS :: CNetwork -> [Text]
            generateJS (CNSequence cn)  = ["var model = tf.sequential();"] ++ generateJS cn
            generateJS (CNCons cn1 cn2) = (generateJS cn1) ++ (generateJS cn2)
            generateJS CNNil = []
            generateJS CNReturn = []
            generateJS (CNLayer layer params) =
                [format
                    ("model.add(tf.layers." % string % "(" % string % "));")
                    (generateName l layer)
                    (paramsToJS params)
                ]
            generateJS _ = [""]

    generateFile l net =
        startCode `append` (generate l net) `append` endCode
        where
            startCode :: Text
            startCode = T.intercalate "\n"
                [ "// Autogenerated code"
                , "var tf = require(\"@tensorflow/tfjs\");"
                , "function model() {"
                , "\n"
                ]

            endCode :: Text
            endCode = T.intercalate "\n"
                [ "\n"
                , "return model;"
                , "}"
                , "\n"
                , "module.exports = model();"
                ]

-- | Converts a map to a parameter object in JavaScript
paramsToJS :: Map String String -> String
paramsToJS m =
    (foldrWithKey showParam "{ " m) ++ "}"
    where
        showParam :: String -> String -> String -> String
        showParam key value accum = accum ++ (camel key) ++ ": " ++ value ++ ", "

instance Generator Python where
    generate l =
        T.intercalate "\n" . generatePy . toCNetwork
        where
            generatePy :: CNetwork -> [Text]
            generatePy (CNSequence cn)  = ["model = tf.keras.models.Sequential()"] ++ generatePy cn
            generatePy (CNCons cn1 cn2) = (generatePy cn1) ++ (generatePy cn2)
            generatePy CNNil = []
            generatePy CNReturn = []
            generatePy (CNLayer layer params) =
                [format
                    ("model.add(tf.layers." % string % "(" % string % "))")
                    (generateName l layer)
                    (paramsToPython params)]
            generatePy _ = [""]

    generateFile l cn =
        startCode `append` (generate l cn)
        where
            startCode :: Text
            startCode = T.intercalate "\n"
                [ "// Autogenerated code"
                , "import tensorflow as tf"
                , "\n"
                ]

-- | Converts a map to keyword arguments in Python
paramsToPython :: Map String String -> String
paramsToPython =
    foldrWithKey showParam ""
    where
        showParam :: String -> String -> String -> String
        showParam key value accum = accum ++ (transform key) ++ "=" ++ value ++ ", "

        -- | Translates keys to python keys of layers
        --
        --   There are some minor changes in names of keys for layers with respect to JS.
        --   Those changes should be delcared here. For most of the keys, transforming them to
        --   snake case does the trick.
        transform :: String -> String
        transform key
            | key == "inputDim" = "input_shape"
            | otherwise         = quietSnake key
