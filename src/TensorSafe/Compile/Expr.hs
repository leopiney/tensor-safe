{-# LANGUAGE OverloadedStrings #-}

-- | This module describes the expression structure of a INetwork instance.
-- -- The INetwork can be structured into a Data structure called CNetwork, with which later
-- -- to compilation external languages can be done.
module TensorSafe.Compile.Expr
  ( DLayer (..),
    CNetwork (..),
    JavaScript (..),
    Python (..),
    Generator,
    generate,
    generateFile,
  )
where

import Data.Map
import Data.Text.Lazy as T
import Formatting
import Text.Casing (camel, quietSnake)

-- | Auxiliary data representation of Layers
-- IMPORTANT: If you add new Layers definitions to `TensorSafe.Layers`, you should add
-- the corresponding data structure here for the same layer.
data DLayer
  = DActivation
  | DBatchNormalization
  | DConcatenate
  | DConv2D
  | DDense
  | DDropout
  | DFlatten
  | DGlobalAvgPooling2D
  | DInput
  | DLSTM
  | DMaxPooling
  | DRelu
  | DZeroPadding2D
  deriving (Show)

-- | Defines the
data CNetwork
  = CNSequence (Map String String) CNetwork
  | CNConcatenate CNetwork CNetwork
  | CNCons CNetwork CNetwork
  | CNLayer DLayer (Map String String)
  | CNReturn -- End of initial sequence network
  | CNNil -- End of possible nested sequence networks
  deriving (Show)

-- | Support for JavaScript compilation
data JavaScript = JavaScript deriving (Show)

-- | Support for Python compilation
data Python = Python deriving (Show)

-- | Defines how are the layers going to be translated to the domain language
-- This translates DLayer to String for each supported language
class LayerGenerator l where
  generateName :: l -> DLayer -> String

instance LayerGenerator JavaScript where
  generateName _ DActivation = "activation"
  generateName _ DBatchNormalization = "batchNormalization"
  generateName _ DConcatenate = "concatenate"
  generateName _ DConv2D = "conv2d"
  generateName _ DDense = "dense"
  generateName _ DDropout = "dropout"
  generateName _ DFlatten = "flatten"
  generateName _ DGlobalAvgPooling2D = "globalAvgeragePooling2D"
  generateName _ DInput = "input"
  generateName _ DLSTM = "lstm"
  generateName _ DMaxPooling = "maxPooling2d"
  generateName _ DRelu = "reLU"
  generateName _ DZeroPadding2D = "zeroPadding2D"

instance LayerGenerator Python where
  generateName _ DActivation = "Activation"
  generateName _ DBatchNormalization = "BatchNormalization"
  generateName _ DConcatenate = "Concatenate"
  generateName _ DConv2D = "Conv2D"
  generateName _ DDense = "Dense"
  generateName _ DDropout = "Dropout"
  generateName _ DFlatten = "Flatten"
  generateName _ DGlobalAvgPooling2D = "GlobalAvgeragePooling2D"
  generateName _ DInput = "Input"
  generateName _ DLSTM = "LSTM"
  generateName _ DMaxPooling = "MaxPool2D"
  generateName _ DRelu = "ReLu"
  generateName _ DZeroPadding2D = "ZeroPadding2D"

-- | Class that defines which languages are supported for CNetworks generation to text
class Generator l where
  -- | Adds supports for a language. Generates a CNetwork to Text
  generate :: l -> CNetwork -> Text

  -- | Similar to 'generate', but also adds necessary header and module lines of text so as to
  -- have the CNetwork compiled at a separate file.
  generateFile :: l -> CNetwork -> Text

data Model = Model String Integer

instance Show Model where
  show (Model name level) = name ++ "_" ++ show level

newModel :: Model
newModel = Model "x" 0

nextModel :: String -> Model -> Model
nextModel name (Model _ level) = Model name (level + 1)

instance Generator JavaScript where
  generate l =
    T.intercalate "\n" . generateJS newModel
    where
      generateJS :: Model -> CNetwork -> [Text]
      generateJS model (CNSequence params cn) =
        format ("var input = tf.input(" % string % ");") (paramsToJS params) :
        format ("var " % string % " = input;") (show model) :
        generateJS model cn
          ++ [ format
                 ("model = tf.model({ inputs: input, outputs: " % string % " });")
                 (show model)
             ]
      generateJS model (CNConcatenate cn1 cn2) =
        let modelA = nextModel "a" model
            modelB = nextModel "b" model
         in format ("var " % string % " = " % string % ";") (show modelA) (show model) :
            generateJS modelA cn1
              ++ format ("var " % string % " = " % string % ";") (show modelB) (show model) :
            generateJS modelB cn2
              ++ [ format
                     (string % " = tf.layers.concatenate().apply([" % string % ", " % string % "])")
                     (show model)
                     (show modelA)
                     (show modelB)
                 ]
      generateJS model (CNCons cn1 cn2) = generateJS model cn1 ++ generateJS model cn2
      generateJS _ CNNil = []
      generateJS _ CNReturn = []
      generateJS model (CNLayer layer params) =
        [ format
            (string % " = tf.layers." % string % "(" % string % ").apply(" % string % ")")
            (show model)
            (generateName l layer)
            (paramsToJS params)
            (show model)
        ]

  generateFile l cn =
    startCode `append` generate l cn `append` endCode
    where
      startCode :: Text
      startCode =
        T.intercalate
          "\n"
          [ "// Autogenerated code",
            "var tf = require(\"@tensorflow/tfjs\");",
            "function model() {",
            "\n"
          ]

      endCode :: Text
      endCode =
        T.intercalate
          "\n"
          [ "\n",
            "return model;",
            "}",
            "\n",
            "module.exports = model();"
          ]

-- | Converts a map to a parameter object in JavaScript
paramsToJS :: Map String String -> String
paramsToJS m =
  foldrWithKey showParam "{ " m ++ "}"
  where
    showParam :: String -> String -> String -> String
    showParam key value accum = accum ++ camel key ++ ": " ++ value ++ ", "

instance Generator Python where
  generate l =
    T.intercalate "\n" . generatePy
    where
      generatePy :: CNetwork -> [Text]
      generatePy (CNSequence params cn) = "model = tf.keras.models.Sequential()" : generatePy cn
      generatePy (CNConcatenate cn1 cn2) = generatePy cn1 ++ generatePy cn2 -- FIX
      generatePy (CNCons cn1 cn2) = generatePy cn1 ++ generatePy cn2
      generatePy CNNil = []
      generatePy CNReturn = []
      generatePy (CNLayer layer params) =
        [ format
            ("model.add(tf.layers." % string % "(" % string % "))")
            (generateName l layer)
            (paramsToPython params)
        ]

  generateFile l cn =
    startCode `append` generate l cn
    where
      startCode :: Text
      startCode =
        T.intercalate
          "\n"
          [ "// Autogenerated code",
            "import tensorflow as tf",
            "\n"
          ]

-- | Converts a map to keyword arguments in Python
paramsToPython :: Map String String -> String
paramsToPython =
  foldrWithKey showParam ""
  where
    showParam :: String -> String -> String -> String
    showParam key value accum = accum ++ transform key ++ "=" ++ value ++ ", "
    transform :: String -> String
    transform key
      | key == "inputDim" = "input_shape"
      | otherwise = quietSnake key
