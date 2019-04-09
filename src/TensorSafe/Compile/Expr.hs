{-# LANGUAGE OverloadedStrings #-}
module TensorSafe.Compile.Expr where

import           Data.Map
import           Data.Text.Lazy as T
import           Formatting


data CNetwork = CNSequence CNetwork
              | CNCons CNetwork CNetwork
              | CNLayer String (Map String String)
              | CNReturn
              | CNNil
              deriving Show


data JavaScript = JavaScript deriving Show

-- | Class that defines which languages are supported for CNetworks generation to text
class Generator l where
    generate :: l -> CNetwork -> Text

-- | Instance for JavaScript generation
instance Generator JavaScript where
    generate _ =
        T.intercalate "\n" . evalJS
        where
            evalJS :: CNetwork -> [Text]
            evalJS (CNSequence cn)  = ["const model = tf.sequential();"] ++ evalJS cn
            evalJS (CNCons cn1 cn2) = (evalJS cn1) ++ (evalJS cn2)
            evalJS (CNNil) = []
            evalJS CNReturn = [] -- ["return model"]
            evalJS (CNLayer layer params) =
                [format ("model.add(tf.layers." % string % "(" % string % "))") layer (paramsToJS params)]


paramsToJS :: Map String String -> String
paramsToJS m =
    (foldrWithKey showParam "{ " m) ++ "}"
    where
        showParam :: String -> String -> String -> String
        showParam key value accum = accum ++ key ++ ": " ++ value ++ ", "
